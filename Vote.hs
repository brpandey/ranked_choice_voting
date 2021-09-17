{- LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer (runWriter)
import Data.Either
import qualified Data.List as List
import Data.Maybe
import System.Environment
import System.Random
import Text.Printf

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Voting.Ballot
import Voting.Candidate
import Voting.Utility
import Voting.Writer

type CandSet = Set Candidate

type BallotAcc = (Ballot, CandSet)

type BallotStore = Map Candidate [Ballot]

type MaybeC = Maybe Candidate

type RoundLeader = (Float, Candidate, Int)

type TallyStore = Map Candidate Int

type VoteResult = ([Candidate], [String])

type VoteState = (TallyStore, BallotStore, [Ballot])

uniformWeight = 20

emptyWeight = 3

defaultCount = "11"

upperPrintCount = 50

main :: IO ()
main = do
  args <- getArgs -- Extract optional arg of ballot count
  let first = hd args
      first' = maybe defaultCount id first
      count = read first' :: Int
      pFlag =
        if count <= upperPrintCount
          then True
          else False
  startGen <- getStdGen
  -- Our workflow is 1) vote 2) classify 3) tally
  -- Grab result from stateful computation, display then 
  -- classify, tally data, display winner
  let votes = evalState (vote count) startGen
  displayVotes pFlag votes
  displayWinner . tally pFlag $ classify votes

-------------------------------
-- IO HELPER DISPLAY FUNCTIONS 
-------------------------------
displayVotes :: Bool -> [Ballot] -> IO ()
displayVotes False votes = do
  putStrLn ""
displayVotes True votes = do
  putStr "~Ballot Votes:"
  putStrLn $ prettyShow votes False
  putStrLn ""

displayWinner :: VoteResult -> IO ()
displayWinner (winnerList, logList) = do
  putStr $ show $ winnerList
  putStr " "
  putStrLn $ List.intercalate "\n" logList

---------------------------
-- VOTE RELATED FUNCTIONS
---------------------------
-- Vote in bulk size, randomly producing ballots
vote :: Int -> State StdGen [Ballot]
vote size = do
  ballot <- mapM generateBallot [1 .. size]
  return ballot

--Generate single voter ballot randomly while occasionally simulating
--where a voter has not specified all n ballot candidates leaving only a
--partial list of top 3 picks or only top 2, for example
--Set is used to keep track of candidates we haven't selected thus far
generateBallot :: Int -> State StdGen Ballot
generateBallot id = do
  let newBallot = emptyBallot id
  result <- runExceptT $ foldM genHelper (newBallot, candSet) [1 .. numCand + 1]
  return $ fromLeft newBallot result
      --Fold helper to grab the next candidate ranked choice value and update the single ballot.
  where
    genHelper :: BallotAcc -> Int -> ExceptT Ballot (State StdGen) BallotAcc
    genHelper (ballotAcc, setAcc) pos = do
      nextValue <- lift $ nextChoice pos setAcc
      ExceptT $ return $ ballotUpdate pos nextValue ballotAcc setAcc

--Helper function to build up the ranked choice candidate selections for a single ballot
ballotUpdate ::
     Int -> Maybe Candidate -> Ballot -> CandSet -> Either Ballot BallotAcc
ballotUpdate pos maybeC ballot set =
  case maybeC of
    Nothing -> Left ballot -- Stop populating ballot, if last cand is empty
    Just c ->
      let ballot' = updateBallotVotes ballot pos c -- Set ballot vote choice pos
          set' = Set.delete c set
          end = numCand + 1
          -- If we have more choices to populate, continue else exit
       in if pos == end
            then Left ballot'
            else Right (ballot', set')

--Randomly picks the next ranked choice candidate
nextChoice :: Int -> CandSet -> State StdGen (Maybe Candidate)
nextChoice pos set = do
  let l = Set.toList set
   -- Loop via recursion until we get a nonempty candidate for first round
  helper pos l
  where
    helper pos l = do
      cand <- randomWeighted l
      if (pos == 1 && cand == Nothing)
        then helper pos l -- Redo if first slot is empty
        else return cand

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
  return $ choose target sumOfWeights -- Find matching candidate based on target

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
classify :: [Ballot] -> (Int, TallyStore, BallotStore, RoundLeader)
classify l = splitter . unwrap $ foldM reduce1 Map.empty l
  where
    pos = 1
    unwrap = maybe Map.empty id
    --Store ballots won in first round, indexed by candidate
    --Every ballot must have a first round choice
    reduce1 :: BallotStore -> Ballot -> Maybe BallotStore
    reduce1 acc b -- (a -> b) -> f a -> f b
     = (\k -> Map.insertWith (++) k [b] acc) <$> (Map.lookup pos $ votes b)
    -- Record total candidate votes and votes per candidate
    reduce2 (totalAcc, countsAcc) k v =
      let size = length v
          countsAcc' = Map.insert k size countsAcc
       in if size > 0
            then (size + totalAcc, countsAcc')
            else (totalAcc, countsAcc')
     -- Allows us to stash acc, pass it to reduce2 and recombine it with results
    splitter acc =
      let (x, y) = Map.foldlWithKey reduce2 (0, Map.empty) acc
          unwrap = maybe (A, 0) id
          (maxKey, maxValue) = unwrap $ hd $ maxCompare y -- Compute top vote leader
          percentage = percent maxValue x
       in (,,,) x y acc (percentage, maxKey, maxValue)

---------------------------
-- TALLY FUNCTIONS
---------------------------
--When a candidate receives more than 50% of all first round votes, they win
--If not, proceed to round to round distribution tally where we converge to the
--last two candidates
tally :: Bool -> (Int, TallyStore, BallotStore, RoundLeader) -> VoteResult
tally pFlag (total, counts, ballots, (percentage, maxKey, maxValue))
  | percentage > 50.0 = winnerOutright $ (,,,) maxKey maxValue total counts
  | otherwise =
    winnerAll pFlag total $ execState (converge) (counts, ballots, [])

--When a candidate receives more than 50% of all first round votes, they win
winnerOutright :: (Candidate, Int, Int, TallyStore) -> VoteResult
winnerOutright x = runWriter $ winnerOutrightWithLog x

--When two candidates are left after each loser in each successive round
--has given up their ballots, we determine which of the two candidates
--has won taking into consideration it may be a draw as well
winnerAll :: Bool -> Int -> VoteState -> VoteResult
winnerAll printFlag total (counts, ballots, discards) =
  runWriter $
  case maxCompare counts of
    [(k1, v1), (k2, v2)] ->
      if v1 == v2
        then winnerDrawWithLog k1 k2 v1 total counts
        else let unwrap = maybe [] id
                 wins = unwrap $ Map.lookup k1 ballots
              in winnerAllWithLog k1 v1 total counts discards wins printFlag
    other -> noWinnerWithLog other

--First find the losing candidate's ballots in each round then distribute their votes
--to that ballots round + 1 choice if available.  If not clear to what new candidate ballot should be
--redistributed to, we don't transfer it and it doesn't get counted again.
--Process continues until final two candidates are left.
converge :: State (VoteState) ()
converge = do
  (c, _, _) <- get
  let size = Map.size c
  mapM_ run [1 .. size - 2]
  where
    run round =
      runMaybeT $ do
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
lowestTally =
  MaybeT $ do
    (c, _, _) <- get
    return $ hd $ minCompare c

lookupValue :: Candidate -> MaybeT (State VoteState) [Ballot]
lookupValue discardKey =
  MaybeT $ do
    (_, b, _) <- get
    return $ Map.lookup discardKey b

--Actual function to remove ballot from Candidate X to Y, annotating ballot of its movement
redistribute :: Candidate -> [Ballot] -> Int -> State (VoteState) ()
redistribute discardKey discards round = do
  mapM_ transfer discards
  where
    transfer ballot =
      runMaybeT $ do
        newKey <- lookupNextRound round discardKey ballot
        lift $ update newKey ballot
    -- Put ballot into new candidates ballot state and update corresponding tally numbers
    update :: Candidate -> Ballot -> State VoteState ()
    update newKey ballot = do
      (c, b, d) <- get
      let pat = "%d: From %s to Pick %d=%s"
          str = printf pat (round) (show discardKey) (round + 1) (show newKey)
          ballot' = updateBallotHist ballot str
          b' = Map.insertWith (++) newKey [ballot'] b -- first update content
          c' = Map.insertWith (+) newKey 1 c -- then update tally
      put (c', b', d)
      return ()

--Helper function to retrieve ballot's next round candidate pick
lookupNextRound ::
     Int -> Candidate -> Ballot -> MaybeT (State VoteState) Candidate
lookupNextRound round discardKey ballot =
  MaybeT $ do
    (c, _, _) <- get
    let voteMap = votes ballot
        round' = round + 1
        value = Map.lookup round' voteMap
  -- Tack on another lookup check using bind to ensure next transfer candidate is still active/valid
    let value' =
          value >>=
          (\x ->
             if Map.member x c
               then Just x
               else Nothing)
    updateDiscard value value' discardKey ballot round -- annotate and store ballot if we can't redistribute
    return $ value'

--Helper functions to annotate and store discarded ballots that aren't able to be redistributed
--(Need both Maybe Candidate values to determine appropriate case)
updateDiscard ::
     MaybeC -> MaybeC -> Candidate -> Ballot -> Int -> State (VoteState) ()
updateDiscard Nothing _ key ballot round = do
  let pattern = "%d: From %s to Discard, No further choices specified"
      str = printf pattern (round) (show key)
  updateDiscard' str ballot
updateDiscard (Just nextKey) Nothing key ballot round = do
  let pattern = "%d: From %s to Discard, Pick %d=%s Not Active"
      str = printf pattern (round) (show key) (round + 1) (show nextKey)
  updateDiscard' str ballot
updateDiscard _ _ _ _ _ = do
  return ()

--Helper to the helper functions, prepending annotated ballot to the discard list 
updateDiscard' :: String -> Ballot -> State (VoteState) ()
updateDiscard' str ballot = do
  (c, b, d) <- get
  let ballot' = updateBallotHist ballot str
  put (c, b, ballot' : d)
  return ()
