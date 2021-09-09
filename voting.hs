{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import System.Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Either
import Data.Maybe
import Text.Printf
import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

data Candidate = A | B | C | D | E deriving (Bounded, Enum, Read, Show, Eq, Ord)


type BallotId = Int
type VoteMap = Map Int Candidate
type CandSet = Set Candidate
type WriteResult = Writer [String] [Candidate]
type VoteResult = ([Candidate], [String])

data Ballot = Ballot {
  uid :: BallotId,
  votes :: VoteMap,
  history :: [String]
--} deriving (Show)
}

--class PrettyShow a where
--    dump :: a -> String

instance Show Ballot where
    show b = "Ballot {"
                 ++ "uid: " ++  shows (uid b) ", "
                 ++ "votes: " ++ shows (Map.toList (votes b)) ", "
                 ++ "history: " ++ "" ++ (List.intercalate ") ->" (reverse (history b))) ++ ""
                 ++ "}"



type TallyStore = Map Candidate Int
type BallotStore = Map Candidate [Ballot]

emptyBallot id = Ballot {uid = id, votes = Map.empty, history = []}

updateBallotVotes :: Ballot -> Int -> Candidate -> Ballot
updateBallotVotes b 1 c = 
 let b' = updateBallotVotes' b 1 c
 in updateBallotHist b' $ printf "%s" (show c)
updateBallotVotes b p c = updateBallotVotes' b p c

updateBallotVotes' :: Ballot -> Int -> Candidate -> Ballot
updateBallotVotes' b pos c = 
  let voteMap = votes b                                                                                                                          
      voteMap' = Map.insert pos c voteMap                                                                                                                                             
  in b { votes = voteMap' }

updateBallotHist :: Ballot -> String -> Ballot
updateBallotHist b x =
  let xs = history b
  in b { history = (x:xs) }

candidateSet = Set.fromList [A .. E]

uniformWeight = 20
emptyWeight = 3

numCandidates = fromEnum (maxBound :: Candidate)
ballotCount = 11

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The args are"
  mapM putStrLn args
  putStrLn "The program name is"
  putStrLn progName
  genInit <- getStdGen

  let votes = evalState (vote ballotCount) genInit

  putStrLn "BALLOT VOTES\n"
  putStrLn $ prettyShow votes
  
  putStrLn "\nCLASSIFY DATA\n"

  let classified = classify votes
  putStrLn $ show $ classified

  putStrLn "\n"

  let (winnerList, logList) = tally classified
  putStr $ show $ winnerList
  putStr " "
  putStrLn $ List.intercalate "\n~" logList


---------------------------
-- VOTE RELATED FUNCTIONS
---------------------------

vote :: Int -> State StdGen [Ballot]
vote size = do
  ballot <- mapM generateBallot [1..size]
  return ballot

--Generate single voter ballot randomly while occasionally simulating
--where a voter has not specified all n ballot candidates leaving only a partial list of top 3 picks for example
--generateBallot :: Int -> StdGen -> (Ballot, StdGen)
generateBallot :: Int -> State StdGen Ballot
generateBallot id = do
  let newBallot = emptyBallot id
  result <- foldM genHelper (Right (newBallot, candidateSet)) [1 .. numCandidates + 1]
  return $ fromLeft newBallot result

--Great place for a monad transformer
genHelper :: (Either (Ballot) (Ballot, CandSet)) -> Int -> State StdGen (Either (Ballot) (Ballot, CandSet))
genHelper (Right (ballotAcc, setAcc)) pos = do
  nextValue <- nextChoice pos setAcc
  return $ ballotUpdateHelper pos nextValue ballotAcc setAcc

genHelper (Left b) pos = do
  return $ Left b

ballotUpdateHelper :: Int -> Maybe Candidate -> Ballot -> CandSet -> Either (Ballot) (Ballot, CandSet)
ballotUpdateHelper pos maybeC ballot set =
  case maybeC of
    Nothing -> Left ballot
    Just c -> 
      let ballot' = updateBallotVotes ballot pos c
          set' = Set.delete c set
          end = numCandidates + 1
      in if pos == end then Left ballot' else Right (ballot', set')

nextChoice :: Int -> CandSet -> State StdGen (Maybe Candidate)
nextChoice pos set = do
  let l = Set.toList set 
  cand <- randomWeighted l
   -- If 1st position is empty, redo!
  ifM (return $ pos == 1 && cand == Nothing) (randomWeighted l) (return cand) 

--Produces a random candidate value given weights
randomWeighted :: [Candidate] -> State StdGen (Maybe Candidate)
randomWeighted c = do
  let l1 = Just <$> c
  let l2 = zip l1 $ cycle [uniformWeight] -- Construct tuple list of candidate and weights
  let l3 = [(Nothing, emptyWeight)] ++ l2 -- Add unchosen candidate option as well 
  let sumOfWeights = scanl1 (\(_, acc) (c, w) -> (c, w + acc)) l3 -- Generate accum list
  let (_, maxWeight) = last sumOfWeights
  gen <- get
  let (target, gen') = randomR (1 :: Int, maxWeight) gen -- Use max as limit in RNG
  put gen'
  let c' = choose target sumOfWeights -- Find matching candidate based on target
  return c'


--Given a target weighted value selects the appropriate candidate
choose :: (Num n, Ord n) => n -> [(Maybe Candidate, n)] -> Maybe Candidate
choose target l = fromLeft (Nothing) $ foldM discardBallotsove target l
  where
    discardBallotsove acc (c, n)
      | n >= acc = Left c
      | otherwise = Right acc

---------------------------
-- CLASSIFY FUNCTION
---------------------------

classify :: [Ballot] -> (Int, TallyStore, BallotStore, (Float, Candidate, Int))
classify = splitter . foldl reduce1 Map.empty
  where
    firstPos = 1
    reduce1 firstChoiceAcc b = -- Store ballots won in first round, indexed by candidate
      case Map.lookup firstPos $ votes b of
        Just firstChoiceCandKey -> Map.insertWith (++) firstChoiceCandKey [b] firstChoiceAcc
        Nothing -> firstChoiceAcc
    reduce2 (totalAcc, countsAcc) k v = -- Record total candidate votes and votes per candidate
      let size = length v
          countsAcc' = Map.insert k size countsAcc
       in if  size > 0 then (size + totalAcc, countsAcc') else (totalAcc, countsAcc')
    splitter acc = -- Allows us to stash acc, pass it to reduce2 and recombine it with results
      let (x,y) = Map.foldlWithKey reduce2 (0, Map.empty) acc
          (maxKey, maxValue) = fromJust $ hd $ maxCompare y
          percentage = percent maxValue x
       in (,,,) x y acc (percentage, maxKey, maxValue)

---------------------------
-- TALLY FUNCTIONS
---------------------------

tally :: (Int, TallyStore, BallotStore, (Float, Candidate, Int)) -> VoteResult
tally (total, counts, ballots, (percentage, maxKey, maxValue))
  | percentage > 50.0 = winnerOutright $ (,,,) maxKey maxValue total counts
  | otherwise = winnerAll total $ converge counts ballots


winnerOutright :: (Candidate, Int, Int, TallyStore) -> VoteResult
winnerOutright x = runWriter $ winnerOutrightWithLog x 

winnerAll :: Int -> (TallyStore, BallotStore) -> VoteResult
winnerAll total (counts, ballots) =
  case maxCompare counts of
    [(k1,v1), (k2, v2)] ->
      if v1 == v2 then 
        runWriter $ winnerDrawWithLog k1 k2 v1 total counts
      else
        runWriter $ winnerAllWithLog k1 v1 total counts $ fromJust $ Map.lookup k1 ballots
    _ -> runWriter $ noWinnerWithLog 

converge :: TallyStore -> BallotStore -> (TallyStore, BallotStore)
converge counts ballots = foldl reduce (counts, ballots) [1..size-2]
  where
    size = Map.size counts
    reduce (countsAcc, ballotsAcc) round =
      let discardKey = fst $ fromJust $ hd $ minCompare countsAcc
          discardBallots = Map.lookup discardKey ballotsAcc
          ballotsAcc' = Map.delete discardKey ballotsAcc
          countsAcc' = Map.delete discardKey countsAcc
      in  redistribute (discardKey, discardBallots) round countsAcc' ballotsAcc'

redistribute :: (Candidate, Maybe [Ballot]) -> Int -> TallyStore -> BallotStore -> (TallyStore, BallotStore)
redistribute (_, Nothing) _ c b = (c,b)
redistribute (discardKey, Just discards) round counts ballots = foldl reduce (counts, ballots) discards
  where
--    updateKeyState :: Maybe Candidate -> Ballot -> TallyStore ->  BallotStore -> (TallyStore, BallotStore)
    updateKeyState Nothing _ s1 s2 = (s1, s2)
    updateKeyState (Just newKey) ballot s1 s2 =
      if Map.member newKey s1 then -- Proceed only if key is a valid and active candidate
        let str =  printf "%d: %s Out, Move to Pick %d=%s" (round) (show discardKey) (round+1) (show newKey)
            ballot' = updateBallotHist ballot str
            s2' = Map.insertWith (++) newKey [ballot'] s2 -- first update content, then the tally
            s1' = Map.insertWith (+) newKey 1 s1
        in (s1', s2')
      else (s1, s2)
      -- Good place to indicate ballots that weren't able to be transfered,
      -- store a discard pile or BallotDiscardStore
--    reduce :: (TallyStore, BallotStore) -> Ballot -> (TallyStore, BallotStore)
    reduce (countsAcc, ballotsAcc) ballot =
      let voteMap = votes ballot
          round' = round + 1
          newKey = Map.lookup round' voteMap
       in updateKeyState newKey ballot countsAcc ballotsAcc


---------------------------
  -- Helper Functions
---------------------------

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t f  = p >>= (\p' -> if p' then t else f)

hd (x:xs) = Just x
hd [] = Nothing

tupCompare :: Ordering -> TallyStore -> [(Candidate, Int)]
tupCompare o m = List.sortBy (\(_,v1)(_,v2) -> compare' o v1 v2) $ Map.toList m
  where
    compare' GT a b = compare b a
    compare' LT a b = compare a b

minCompare :: TallyStore -> [(Candidate, Int)]
minCompare = tupCompare LT

maxCompare :: TallyStore -> [(Candidate, Int)]
maxCompare = tupCompare GT

percent :: (Integral n) => n -> n -> Float
percent a b = 100 * (fromIntegral a) / (fromIntegral b)

prettyShow :: [Ballot] -> String
prettyShow votes = List.intercalate "\n" $ map show votes


--let str = printf "%d: %s to %s" (round) (show discardKey) (show key)
-- transfer ballot round oldKey newKey = writer (ballot)



---------------------------
  -- Writer Functions
---------------------------


logWinnerOutright :: Candidate -> WriteResult
logWinnerOutright key =
  let str = "WINNER! Candidate (%s) has won outright in 1st round by over 50 percent!"
  in writer ([key], [printf str (show key)])

logWinnerDraw :: Candidate -> Candidate -> WriteResult
logWinnerDraw k1 k2 =
  let str = "DRAW! Both candidates (%s) and (%s) have the same number of votes!"
  in writer ([k1,k2], [printf str (show k1) (show k2)])

logWinnerAll :: Candidate -> WriteResult
logWinnerAll key =
  let str = "WINNER!!! Candidate %s has the most votes!"
  in writer ([key], [printf str (show key)])

logPercent :: Int -> Int -> WriteResult
logPercent value total = writer ([], [printf "Percentage -> %.3f percent votes!" (percent value total)])

logCounts :: TallyStore -> Int -> WriteResult
logCounts counts total = writer ([], [printf "Counts -> %s out of %d total" (show $ maxCompare counts) (total)])

logBallots :: [Ballot] -> WriteResult
logBallots list = writer ([], [printf "Winning Ballots: \n\n%s" (prettyShow list)])

winnerOutrightWithLog :: (Candidate, Int, Int, TallyStore) -> WriteResult
winnerOutrightWithLog (key, value, total, counts) = do
  winner <- logWinnerOutright key
  tell ["Note: (Further rounds ingored as 1st round results prove a decisive outcome)"]
  logPercent value total
  logCounts counts total
  return (winner)

winnerAllWithLog :: Candidate -> Int -> Int -> TallyStore -> [Ballot] -> WriteResult
winnerAllWithLog key value total counts list = do
  winner <- logWinnerAll key
  logPercent value total
  logCounts counts total
  tell ["Note: (After all rounds counted, candidate with most votes wins EVEN if less than 50 percent)"]
  logBallots list
  return (winner)

winnerDrawWithLog :: Candidate -> Candidate -> Int -> Int -> TallyStore -> WriteResult
winnerDrawWithLog k1 k2 value total counts = do
  winner <- logWinnerDraw k1 k2
  tell ["(A split governing future awaits)"]
  logPercent value total
  logCounts counts total
  return (winner)

noWinnerWithLog :: WriteResult
noWinnerWithLog = do
  tell ["NO WINNER?! Something went wrong! Did someone pay the power bill?"]
  return ([])
