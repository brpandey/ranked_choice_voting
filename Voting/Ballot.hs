module Voting.Ballot
  ( Ballot(..)
  , emptyBallot
  , updateBallotVotes
  , updateBallotHist
  ) where

import Text.Printf
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import Voting.Candidate

type BallotId = Int
type VoteMap = Map Int Candidate

---------------------------
-- Ballot Functionality
---------------------------

data Ballot = Ballot {
  uid :: BallotId,
  votes :: VoteMap,
  history :: [String]
}

instance Show Ballot where
  show b = showBallot b

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

showBallot :: Ballot -> String
showBallot b = "Ballot {"
                 ++ "uid: " ++  shows (uid b) ", "
                 ++ "votes: " ++ shows (Map.toList (votes b)) ", "
                 ++ "candidate history: " ++ "[" ++ (List.intercalate " -> " (reverse (history b))) ++ "]"
                 ++ "}"

