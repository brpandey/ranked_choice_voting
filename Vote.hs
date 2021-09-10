{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import System.Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer (runWriter)
import Control.Monad.Except

import Data.Either
import Data.Maybe
import Text.Printf
import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Voting.Candidate
import Voting.Ballot
import Voting.Writer
import Voting.Utility

type CandSet = Set Candidate
type VoteResult = ([Candidate], [String])
type TallyStore = Map Candidate Int
type BallotStore = Map Candidate [Ballot]
type VoteState = (TallyStore, BallotStore, [Ballot])

uniformWeight = 20
emptyWeight = 3
defaultCount = "11"
upperPrintCount = 50

main :: IO ()
main = do
  args <- getArgs
  let first = hd args
      first' = maybe defaultCount id first
      count = read first' :: Int
      printFlag = if count <= upperPrintCount then True else False

  startGen <- getStdGen

  let votes = evalState (vote count) startGen

  case printFlag of
    True -> do
      putStr "~Ballot Votes:"
      putStrLn $ prettyShow votes False
      putStrLn ""
    False -> do putStr ""

  let (winnerList, logList) = tally printFlag $ classify votes
  putStr $ show $ winnerList
  putStr " "
  putStrLn $ List.intercalate "\n" logList


---------------------------
-- VOTE RELATED FUNCTIONS
---------------------------

-- Vote in bulk size, randomly producing ballots
vote :: Int -> State StdGen [Ballot]
vote size = do
  ballot <- mapM generateBallot [1..size]
  return ballot

--Generate single voter ballot randomly while occasionally simulating
--where a voter has not specified all n ballot candidates leaving only a
--partial list of top 3 picks or only top 2, for example
--Set is used to keep track of candidates we haven't selected thus far
generateBallot :: Int -> State StdGen Ballot
generateBallot id = do
  let newBallot = emptyBallot id
  result <- runExceptT $ foldM genHelper (newBallot, candidateSet) [1..numCandidates+1]
  return $ fromLeft newBallot result
    where
      --Fold helper to grab the next candidate ranked choice value and update the single ballot.
      genHelper :: (Ballot, CandSet) -> Int -> ExceptT Ballot (State StdGen) (Ballot, CandSet)
      genHelper (ballotAcc, setAcc) pos = do
        nextValue <- lift $ nextChoice pos setAcc
        ExceptT $ return $ ballotUpdateHelper pos nextValue ballotAcc setAcc

--Helper function to build up the ranked choice candidate selections for a single ballot
ballotUpdateHelper :: Int -> Maybe Candidate -> Ballot -> CandSet -> Either (Ballot) (Ballot, CandSet)
ballotUpdateHelper pos maybeC ballot set =
  case maybeC of
    Nothing -> Left ballot -- If we have any empty candidate choice stop populating ballot anymore
    Just c -> 
      let ballot' = updateBallotVotes ballot pos c -- Set ballot vote choice pos
          set' = Set.delete c set
          end = numCandidates + 1
          -- If we have more choice to populate continue else exit
      in if pos == end then Left ballot' else Right (ballot', set')

--Randomly picks theh next ranked choice candidate
nextChoice :: Int -> CandSet -> State StdGen (Maybe Candidate)
nextChoice pos set = do
  let l = Set.toList set 
  cand <- randomWeighted l -- randomly generate next candidate
   -- If 1st position is empty, redo!
  ifM (return $ pos == 1 && cand == Nothing) (randomWeighted l) (return cand)


--Produces a random candidate value given weights over the candidates
randomWeighted :: [Candidate] -> State StdGen (Maybe Candidate)
randomWeighted c = do
  let l1 = Just <$> c
      l2 = zip l1 $ cycle [uniformWeight] -- Construct tuple list of candidate and weights
      l3 = [(Nothing, emptyWeight)] ++ l2 -- Add unchosen candidate option as well 
      sumOfWeights = scanl1 (\(_, acc) (c, w) -> (c, w + acc)) l3 -- Generate accum list
      (_, maxWeight) = last sumOfWeights
  gen <- get
  let (target, gen') = randomR (1 :: Int, maxWeight) gen -- Use max as limit in RNG
  put gen'
  let c' = choose target sumOfWeights -- Find matching candidate based on target
  return c'

--Given a target weighted value selects the appropriate candidate
choose :: (Num n, Ord n) => n -> [(Maybe Candidate, n)] -> Maybe Candidate
choose target l = fromLeft (Nothing) $ foldM above target l
  where
    above acc (c, n)
      | n >= acc = Left c
      | otherwise = Right acc

---------------------------
-- CLASSIFY FUNCTION
---------------------------

-- Setup initial bookeeping state to keep track of votes by candidate
-- Each candidate has a list of ballots won in the first round
classify :: [Ballot] -> (Int, TallyStore, BallotStore, (Float, Candidate, Int))
classify = splitter . foldl reduce1 Map.empty
  where
    pos = 1
    reduce1 acc b = -- Store ballots won in first round, indexed by candidate
      case Map.lookup pos $ votes b of
        Just k -> Map.insertWith (++) k [b] acc
        Nothing -> acc
    reduce2 (totalAcc, countsAcc) k v = -- Record total candidate votes and votes per candidate
      let size = length v
          countsAcc' = Map.insert k size countsAcc
       in if size > 0 then (size + totalAcc, countsAcc') else (totalAcc, countsAcc')
    splitter acc = -- Allows us to stash acc, pass it to reduce2 and recombine it with results
      let (x,y) = Map.foldlWithKey reduce2 (0, Map.empty) acc
          (maxKey, maxValue):_ = maxCompare y -- Compute top vote leader
          percentage = percent maxValue x
       in (,,,) x y acc (percentage, maxKey, maxValue)

---------------------------
-- TALLY FUNCTIONS
---------------------------

--When a candidate receives more than 50% of all first round votes, they win
--If not, proceed to round to round distribution tally where we converge to the
--last two candidates
tally :: Bool -> (Int, TallyStore, BallotStore, (Float, Candidate, Int)) -> VoteResult
tally printFlag (total, counts, ballots, (percentage, maxKey, maxValue))
  | percentage > 50.0 = winnerOutright $ (,,,) maxKey maxValue total counts
  | otherwise = winnerAll printFlag total $ execState (converge) (counts, ballots, [])

--When a candidate receives more than 50% of all first round votes, they win
winnerOutright :: (Candidate, Int, Int, TallyStore) -> VoteResult
winnerOutright x = runWriter $ winnerOutrightWithLog x 

--When two candidates are left after each loser in each successive round
--has given up their ballots, we determine which of the two candidates
--has won taking into consideration it may be a draw as well
winnerAll :: Bool -> Int -> VoteState -> VoteResult
winnerAll printFlag total (counts, ballots, discards) =
  case maxCompare counts of
    [(k1,v1), (k2, v2)] ->
      if v1 == v2 then
        runWriter $ winnerDrawWithLog k1 k2 v1 total counts
      else
        let wins = maybe [] id $ Map.lookup k1 ballots
        in runWriter $ winnerAllWithLog k1 v1 total counts discards wins printFlag
    _ -> runWriter $ noWinnerWithLog 

--First find the losing candidate's ballots in each round then distribute their votes
--to that ballots round + 1 choice if available.  If not clear to what new candidate ballot should be
--redistributed to, we don't transfer it and it doesn't get counted again.
--Process continues until final two candidates are left.

converge :: State (VoteState) ()
converge = do
  (c, _, _) <- get
  let size = Map.size c
  mapM_ run [1..size-2]
  where
    run round = runMaybeT $ do
      tally <- lowestTally
      let discardKey = fst tally
      discardBallots <- lookupValue discardKey
      lift $ update discardKey
      lift $ redistribute discardKey discardBallots round
      return ()
    update :: Candidate -> State (VoteState) ()
    update discardKey = do
      (c, b, d) <- get
      let b' = Map.delete discardKey b
          c' = Map.delete discardKey c
      put (c', b', d)
      return ()

--Converge helper functions that use the MaybeT monad
lowestTally :: MaybeT (State VoteState) (Candidate, Int)
lowestTally = MaybeT $ do
  (c, _, _) <- get
  return $ hd $ minCompare c
  
lookupValue :: Candidate -> MaybeT (State VoteState) [Ballot]
lookupValue discardKey = MaybeT $ do
  (_, b, _) <- get
  return $ Map.lookup discardKey b


--Actual function to remove ballot from Candidate X to Y, annotating ballot of its movement
-- Good place to indicate ballots that weren't able to be transfered,
-- store a discard pile or BallotDiscardStore
redistribute :: Candidate -> [Ballot] -> Int -> State (VoteState) ()
redistribute discardKey discards round = do mapM_ combine discards
  where
    combine ballot = runMaybeT $ do
      newKey <- lookupNextRound round discardKey ballot
      lift $ update newKey ballot
    -- Put ballot into new candidates ballot state and update corresponding tally numbers
    update :: Candidate -> Ballot -> State VoteState ()
    update newKey ballot = do
      (c, b, d) <- get
      let str =  printf "%d: From %s to Pick %d=%s" (round) (show discardKey) (round+1) (show newKey)
          ballot' = updateBallotHist ballot str
          b' = Map.insertWith (++) newKey [ballot'] b -- first update content
          c' = Map.insertWith (+) newKey 1 c -- then update tally
      put (c', b', d)
      return ()

--Helper function to retrieve ballot's next round candidate pick
lookupNextRound :: Int -> Candidate -> Ballot -> MaybeT (State VoteState) Candidate
lookupNextRound round discardKey ballot = MaybeT $ do
  (c, _, _) <- get
  let voteMap = votes ballot
      round' = round + 1
      result = Map.lookup round' voteMap
  -- Tack on another lookup check using liftM to ensure next transfer candidate is still active/valid
  let result' = join $ liftM (\x -> if Map.member x c then Just x else Nothing) $ result
  updateDiscard result result' discardKey ballot round -- annotate and store ballot if we can't redistribute
  return $ result'

--Helper functions to annotate and store discarded ballots that aren't able to be redistributed
--(Need both Maybe Candidate values to determine appropriate case)
updateDiscard :: Maybe Candidate -> Maybe Candidate -> Candidate -> Ballot -> Int -> State (VoteState) ()
updateDiscard Nothing _ key ballot round = do
  let str = printf "%d: From %s to Discard, No further choices specified" (round) (show key)
  updateDiscard' str ballot
updateDiscard (Just nextKey) Nothing key ballot round = do
  let str = printf "%d: From %s to Discard, Pick %d=%s Not Active" (round) (show key) (round+1) (show nextKey)
  updateDiscard' str ballot
updateDiscard _ _ _ _ _ = do return ()

--Helper to the helper functions, prepending annotated ballot to the discard list 
updateDiscard' :: String -> Ballot -> State (VoteState) ()
updateDiscard' str ballot = do
  (c, b, d) <- get
  let ballot' = updateBallotHist ballot str
  put (c, b, ballot':d)
  return ()
