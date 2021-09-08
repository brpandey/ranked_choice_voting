import System.Environment
import System.Random
import Control.Monad
import Control.Monad.State
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

data Ballot = Ballot {
  uid :: BallotId,
  votes :: VoteMap
} deriving (Show)


emptyBallot id = Ballot {uid = id, votes = Map.empty}
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
  let vlist = map show votes

  putStrLn $ show vlist
  putStrLn "do the whole list"
  let slist = List.intercalate "\n" vlist

  putStrLn slist

  putStrLn "Waka waka!"

  let classified = classify votes
  putStrLn $ show $ classified

  putStrLn $ show $ tally classified

---------------------------
-- VOTE RELATED FUNCTIONS
---------------------------
  
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t f  = p >>= (\p' -> if p' then t else f)

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
    Just c -> let voteMap = votes ballot 
                  voteMap' = Map.insert pos c voteMap
                  ballot' = ballot { votes = voteMap' }
                  set' = Set.delete c set
                  end = numCandidates + 1
               in -- if end wrap value with Left
                  if pos == end then Left ballot' else Right (ballot', set')

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
  
classify :: [Ballot] -> (Int, Map Candidate Int, Map Candidate [Ballot], (Candidate, Int))
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
          max = fromJust $ hd $ maxTupCompare y 
       in (,,,) x y acc max
    
---------------------------
-- TALLY FUNCTIONS
---------------------------

tally (total, counts, ballots, (maxKey, maxValue))
  | maxValue / total > 0.5 = winnerOutright total maxKey maxValue
  | otherwise = winnerCumulative total $ converge counts ballots


winnerOutright :: Int -> Candidate -> Int -> ([(Int, Float)], [Char])
winnerOutright total maxKey maxValue = ([(maxKey, 100*(maxValue/total)], "WINNER! Candidate has won by over 50% in first round")

winnerCumulative :: Int -> (Map Candidate Int, Map Candidate [Ballot]) -> ([(Int, Float)], [Char])
winnerCumulative total (counts, ballots) =
  case maxCompare counts of
    [(k1,v), (k2,v)] -> 
      let percentage = 100*(v/total)
       in ([(k1, percentage),(k2, percentage)], "DRAW! Both candidates have the same number of votes")
    [(k1,v1), (k2, v2)] -> 
      let proof = fromJust $ Map.lookup k1 ballots
          percentage = printf "%0.*f" 4 (100*(v1/total))
          str = printf "WINNER!!! Candidate %s has the most votes with %s!  Winning ballots -> %s" (show k1) (percentage) (show proof)
      in ([(k1, percentage)], str)
    _ -> ([], "No Winner! something went wrong!")

converge :: Map Candidate Int -> Map Candidate [Ballot] -> (Map Candidate Int, Map Candidate [Ballot])
converge counts ballots = Map.foldlWithKey reduce (counts, ballots) [1..size-2]
  where
    size = Map.size counts
    reduce (countsAcc, ballotsAcc) round (c,b) =
      let discardKey = fst $ hd $ fromJust $ minCompare countsAcc
          discardBallots = Map.lookup discardKey ballotsAcc
          ballotsAcc' = Map.delete discardKey ballotsAcc
          countsAcc' = Map.delete discardKey countsAcc
      in  redistribute (discardKey discardBallots) round (countsAcc' ballotsAcc')


redistribute :: (Candidate, Maybe [Ballot]) -> Int -> (Map Candidate Int, Map Candidate [Ballot]) -> (Map Candidate Int, Map Candidate [Ballot])
redistribute (_, Nothing) _ (c b) = (c,b)
redistribute (discardKey, Just discardBallots) round (counts, ballots) = Map.foldlWithKey reduce (counts, ballots) discardBallots
  where
    updateKeyState Nothing _ s1 s2 = (s1, s2)
    updateKeyState Just key value s1 s2 =
        case Map.member key s1 -- Proceed only if key is a valid and an active candidate
          True -> 
            --let str = printf "%d: %s to %s" (round) (show discardKey) (show key)
            let s2' = Map.insertWith (++) key [value] s2 -- first update content, then the tally
                s1' = Map.insertWith (+) key 1 s1
             in (s1', s2')
          False -> (s1, s2)
    reduce (countsAcc, ballotsAcc) ballot =
      let voteMap = votes ballot
          round' = round + 1
          newRecipient = Map.lookup round' voteMap
       in updateKeyState newRecipient ballot countsAcc ballotsAcc


---------------------------
  -- Helper Functions
---------------------------

hd (x:xs) = Just x
hd [] = Nothing

tupCompare :: Ordering -> Map Candidate Int -> [(Candidate, Int)]
tupCompare o m = List.sortBy (\(_,v1)(_,v2) -> compare' o v1 v2) $ Map.toList m
  where
    compare' GT a b = compare b a
    compare' LT a b = compare a b
        

minCompare :: Map Candidate Int -> [(Candidate, Int)]
minCompare = tupCompare LT

maxCompare :: Map Candidate Int -> [(Candidate, Int)]
maxCompare = tupCompare GT


