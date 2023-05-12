{-# LANGUAGE FlexibleContexts #-}
{- |
Copyright    : 2010 Aristid Breitkreuz
License      : BSD3
Stability    : experimental
Portability  : portable

Functions for shuffling elements according to weights.

Definitions:

  [Weight]        A number above /0/ denoting how likely it is for an element to 
                  end up in the first position.

  [Probability]   A weight normalised into the /(0,1]/ range.

  [Weighted list] A list of pairs @(w, a)@, where @w@ is the weight of 
                  element @a@.
                  The probability of an element getting into the first position
                  is equal by its weight divided by the sum of all weights, and
                  the probability of getting into a position other than the 
                  first is equal to the probability of getting in the first 
                  position when all elements in prior positions have been
                  removed from the weighted list.

  [CDF Map]       A map of /summed weights/ to elements. For example, a weighted
                  list @[(0.2, 'a'), (0.6, 'b'), (0.2, 'c')]@ corresponds to a
                  CDF map of @[(0.2, 'a'), (0.8, 'b'), (1.0, 'c')]@ 
                  (as a 'Map'). The weights are summed from left to right.
-}

module WeightedShuffle
(
  -- * Shuffling
  weightedShuffleCDF
, weightedShuffle
  -- * Sampling
, weightedSampleCDF
, weightedSample
  -- * Extraction
, weightedChoiceExtractCDF
  -- * Utilities
, cdfMapFromList
)
where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import qualified Data.Map as M

moduleError :: String -> String -> a
moduleError n s = error $ "Data.Random.Shuffle.Weighted." ++ n ++ ": " ++ s

-- | Randomly shuffle a CDF map according to its weights.
weightedShuffleCDF :: M.Map Double a -> RVar [a]
weightedShuffleCDF m | M.null m  = return []
                     | otherwise = weightedChoiceExtractCDF m >>= \(m', a) -> (a:) <$> weightedShuffleCDF m'

-- | Randomly shuffle a weighted list according to its weights.
weightedShuffle :: [(Double, a)] -> RVar [a]
weightedShuffle = weightedShuffleCDF . cdfMapFromList

-- | Randomly draw /n/ elements from a CDF map according to its weights.
weightedSampleCDF :: Int -> M.Map Double a -> RVar [a]
weightedSampleCDF n m | M.null m || n <= 0 = return []
                      | otherwise          = weightedChoiceExtractCDF m >>= \(m', a) -> (a:) <$> weightedSampleCDF (n - 1) m'

-- | Randomly draw /n/ elements from a weighted list according to its weights.
weightedSample :: Int -> [(Double, a)] -> RVar [a]
weightedSample n = weightedSampleCDF n . cdfMapFromList

-- | Randomly extract an element from a CDF map according to its weights. The
-- element is removed and the resulting "weight gap" closed.
weightedChoiceExtractCDF :: M.Map Double a -> RVar (M.Map Double a, a)
weightedChoiceExtractCDF m | M.null m         = moduleError "weightedChoiceExtractCDF" "empty map"
                           | M.null exceptMax = return (exceptMax, maxE)
                           | otherwise        = extract <$> rvar (Uniform 0 wmax)
    where Just ((wmax, maxE), exceptMax) = M.maxViewWithKey m
          extract w = (a `M.union` M.mapKeysMonotonic (subtract gap) c, b)
              where (a, e, r') = M.splitLookup w m
                    r = case e of
                          Nothing -> r'
                          Just ex -> M.insert w ex r'
                    Just ((k, b), c) = M.minViewWithKey r
                    gap = case M.minViewWithKey c of
                            Nothing -> 0
                            Just ((k2, _), _) -> k2 - k

-- | Generate a CDF map from a weighted list.
cdfMapFromList :: (Num w, Eq w) => [(w, a)] -> M.Map w a
cdfMapFromList = M.fromAscListWith (const id) 
                 . scanl1 (\(w1, _) (w2, x) -> (w1 + w2, x)) 
                 . dropWhile ((==0) . fst)
