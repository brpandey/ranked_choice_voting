module Voting.Utility
  ( hd
  , minCompare
  , maxCompare
  , percent
  , prettyShow
  ) where

import qualified Data.List as List

import Data.Map (Map)
import qualified Data.Map as Map

import Voting.Ballot
import Voting.Candidate

type TallyStore = Map Candidate Int

hd :: [a] -> Maybe a
hd (x:xs) = Just x
hd [] = Nothing

tupCompare :: Ordering -> TallyStore -> [(Candidate, Int)]
tupCompare o m =
  List.sortBy (\(_, v1) (_, v2) -> compare' o v1 v2) $ Map.toList m
  where
    compare' GT a b = compare b a
    compare' LT a b = compare a b

minCompare :: TallyStore -> [(Candidate, Int)]
minCompare = tupCompare LT

maxCompare :: TallyStore -> [(Candidate, Int)]
maxCompare = tupCompare GT

percent :: (Integral n) => n -> n -> Float
percent a b = 100 * (fromIntegral a) / (fromIntegral b)

prettyShow :: [Ballot] -> Bool -> String
prettyShow votes False = concat $ List.intersperse "\n" $ "" : (map show votes)
prettyShow votes True = concat $ List.intersperse "\n  " $ "" : (map show votes)
