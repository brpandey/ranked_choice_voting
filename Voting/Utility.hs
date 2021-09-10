module Voting.Utility
  ( ifM
  , hd
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


ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t f  = p >>= (\p' -> if p' then t else f)

hd (x:xs) = Just x
hd [] = Nothing

tupCompare :: Ordering -> TallyStore -> [(Candidate, Int)]
tupCompare o m = List.sortBy (\(_,v1)(_,v2) -> compare' o v1 v2) $ Map.toList m
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
--prettyShow votes =  List.intercalate "\n\t" $ map show votes
prettyShow votes False = concat $ List.intersperse "\n" $ "":(map show votes)
prettyShow votes True = concat $ List.intersperse "\n  " $ "":(map show votes)
