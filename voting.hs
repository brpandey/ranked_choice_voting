import System.Environment
import System.Random
import Control.Monad
import Control.Monad.State
import Data.Either

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
  choices :: VoteMap
} deriving (Show)

emptyBallot id = Ballot {uid = id, choices = Map.empty}
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

  putStrLn $ show $ classify votes

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
    Just c -> let voteMap = choices ballot 
                  voteMap' = Map.insert pos c voteMap
                  ballot' = ballot { choices = voteMap' }
                  set' = Set.delete c set
               in
                  if pos == numCandidates + 1 then -- if we are at the end
                    Left ballot'
                  else Right (ballot', set')

nextChoice :: Int -> CandSet -> State StdGen (Maybe Candidate)
nextChoice pos set = do
  let l = Set.toList set 
  cand <- randomWeighted l
  ifM (return $ pos == 1 && cand == Nothing) (randomWeighted l) (return cand)   -- If 1st position is empty, redo!

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
choose target l = fromLeft (Nothing) $ foldM above target l
  where
    above acc (c, n)
      | n >= acc = Left c -- Early termination
      | otherwise = Right acc

---------------------------
-- CLASSIFY FUNCTIONS
---------------------------
  
classify :: [Ballot] -> (Int, Map Candidate Int)
classify = Map.foldlWithKey reduce2 (0, Map.empty) . foldl reduce1 Map.empty
  where
    firstPos = 1
    reduce1 acc b = 
      case Map.lookup firstPos $ choices b of
        Just candKey -> Map.insertWith (++) candKey [b] acc
        Nothing -> acc
    reduce2 (totalAcc, countsAcc) k v =
      let size = length v
          countsAcc' = Map.insert k size countsAcc
       in if  size > 0 then (size + totalAcc, countsAcc') else (totalAcc, countsAcc')


