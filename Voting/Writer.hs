module Voting.Writer
  ( winnerOutrightWithLog
  , winnerAllWithLog
  , winnerDrawWithLog
  , noWinnerWithLog
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Writer
import Text.Printf

import Voting.Ballot
import Voting.Candidate
import Voting.Utility


type TallyStore = Map Candidate Int
type WriteResult = Writer [String] [Candidate]


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
logPercent value total = writer ([], [printf "~Percentage -> %.3f percent votes!" (percent value total)])

logCounts :: TallyStore -> Int -> WriteResult
logCounts counts total = writer ([], [printf "~Counts -> %s out of %d total" (show $ maxCompare counts) (total)])

logWinBallots :: [Ballot] -> Bool -> WriteResult
logWinBallots list True = writer ([], [printf "~Winning Ballots: \n%s\n" (prettyShow list)])
logWinBallots list False = writer ([], [printf "~Winning Ballots: <Hidden>\n\n" ])

logDiscardBallots :: [Ballot] -> Bool -> WriteResult
logDiscardBallots list True = writer ([], [printf "~Discarded Ballots: \n%s\n" (prettyShow list)])
logDiscardBallots list False = writer ([], [printf "~Discarded Ballots: <Hidden>\n\n" ])

winnerOutrightWithLog :: (Candidate, Int, Int, TallyStore) -> WriteResult
winnerOutrightWithLog (key, value, total, counts) = do
  winner <- logWinnerOutright key
  tell ["~Note: (Further rounds ingored as 1st round results prove a decisive outcome)"]
  logPercent value total
  logCounts counts total
  return (winner)

winnerAllWithLog :: Candidate -> Int -> Int -> TallyStore -> [Ballot] -> [Ballot] -> Bool -> WriteResult
winnerAllWithLog key value total counts discards wins flag = do
  winner <- logWinnerAll key
  logPercent value total
  logCounts counts total
  tell ["~Note: (After all rounds counted, candidate with most votes wins EVEN if less than 50 percent)"]
  logWinBallots wins flag
  logDiscardBallots discards flag
  return (winner)

winnerDrawWithLog :: Candidate -> Candidate -> Int -> Int -> TallyStore -> WriteResult
winnerDrawWithLog k1 k2 value total counts = do
  winner <- logWinnerDraw k1 k2
  tell ["~Note: (A split governing future awaits)"]
  logPercent value total
  logCounts counts total
  return (winner)

noWinnerWithLog :: WriteResult
noWinnerWithLog = do
  tell ["NO WINNER?! Something went wrong! Did someone pay the power bill?"]
  return ([])