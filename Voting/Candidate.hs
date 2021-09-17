module Voting.Candidate
  ( Candidate(..)
  , candSet
  , numCand
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

candSet :: Set Candidate
candSet = Set.fromList [A .. E]

numCand :: Int
numCand = fromEnum (maxBound :: Candidate)
