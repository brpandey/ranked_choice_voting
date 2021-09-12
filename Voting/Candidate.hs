module Voting.Candidate
  ( Candidate(..)
  , candidateSet
  , numCandidates
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

data Candidate
  = A
  | B
  | C
  | D
  | E
  deriving (Bounded, Enum, Read, Show, Eq, Ord)

candidateSet :: Set Candidate
candidateSet = Set.fromList [A .. E]

numCandidates :: Int
numCandidates = fromEnum (maxBound :: Candidate)
