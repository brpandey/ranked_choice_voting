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


data Ballot = Ballot {
  uid :: BallotId,
  choices :: VoteMap
} deriving (Show)

emptyBallot id = Ballot {uid = id, choices = Map.empty}
candidateSet = Set.fromList [A .. E]

uniformWeight = 20
emptyWeight = 3

numCandidates = fromEnum (maxBound :: Candidate)
ballotCount = 20

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The args are"
  mapM putStrLn args
  putStrLn "The program name is"
  putStrLn progName
  gen <- getStdGen

  let (list, _) = createBallots gen ballotCount
  let tlist = map show list
  
  putStrLn $ show list
  putStrLn "do the whole list"
  let slist = List.intercalate "\n" tlist
  
--  let tally = foldl generateBallot gen [1..ballotCount]
--  let Left randomCand = generateBallot 1 gen
--  putStrLn $ show randomCand
  putStrLn slist

createBallots :: StdGen -> Int -> ([Ballot], StdGen)
createBallots gen size = foldl createHelper ([], gen) [1..size]
  where
    createHelper (listAcc, genAcc) i =
      let (randCandidate, gen') = generateBallot i genAcc
       in (randCandidate:listAcc, gen')

--Generate single voter ballot randomly while occasionally simulating
--where a voter has not specified all n ballot candidates leaving only a partial list of top 3 picks for example
generateBallot :: Int -> StdGen -> (Ballot, StdGen)
generateBallot id gen = fromLeft (emptyBallot id, gen) $ foldM genHelper (Map.empty, candidateSet, gen) [1 .. numCandidates + 1]
  where genHelper (ballotAcc, setAcc, genAcc) pos =
          case nextChoice genAcc pos $ Set.toList setAcc of
            (Nothing, gen') -> Left (Ballot {uid = id, choices = ballotAcc}, gen')
            (Just c, gen') -> let ballotAcc' = Map.insert pos c ballotAcc
                                  setAcc' = Set.delete c setAcc
                              in
                                if pos == numCandidates + 1 then
                                  Left (Ballot {uid = id, choices = ballotAcc'}, gen')
                                else Right (ballotAcc', setAcc', gen')

nextChoice :: StdGen -> Int -> [Candidate] -> (Maybe Candidate, StdGen)
nextChoice gen pos l 
  | (pos == 1 && newC == Nothing) = randomWeighted l newGen -- 1st position shouldn't be empty, redo!
  | otherwise = (newC, newGen)
  where (newC, newGen) = randomWeighted l gen

--Produces a random candidate value given weights
randomWeighted :: [Candidate] -> StdGen -> (Maybe Candidate, StdGen)
randomWeighted c gen =
  let l1 = Just <$> c
      l2 = zip l1 $ cycle [uniformWeight] -- Construct tuple list of candidate and weights
      l3 = [(Nothing, emptyWeight)] ++ l2 -- Add unchosen candidate option as well 
      sumOfWeights = scanl1 (\(_, acc) (c, w) -> (c, w + acc)) l3 -- Generate accum list
      (_, maxWeight) = last sumOfWeights
      (target, newGen) = randomR (1 :: Int, maxWeight) gen -- Use max as limit in RNG
      newC = choose target sumOfWeights -- Find matching candidate based on target
  in (newC, newGen)

--Given a target weighted value selects the appropriate candidate
choose :: (Num n, Ord n) => n -> [(Maybe Candidate, n)] -> Maybe Candidate
choose target l = fromLeft (Nothing) $ foldM above target l
  where
    above targetAcc (c, n)
      | n >= targetAcc = Left c -- Left signifies error, allowing us to break out early with chosen value
      | otherwise = Right targetAcc


