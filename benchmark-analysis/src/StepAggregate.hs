{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module StepAggregate (VariantAggregate(..), stepAggregator) where

import Data.Conduit (ConduitT)
import qualified Data.Conduit.Combinators as C
import Data.Function (on)
import qualified Data.Vector as V
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import Predictor
import Schema
import Sql (Region)
import Query.Step (StepInfo(..))
import Utils.Conduit (foldGroup)
import Utils.ImplTiming (ImplTiming(..), liftImplTiming)
import Utils.Pair (Pair(..))

data VariantAggregate =
  VariantAgg
  { variantid :: {-# UNPACK #-} !(Key Variant)
  , optimalTime :: {-# UNPACK #-} !Double
  , implTimes :: {-# UNPACK #-} !(Pair (Vector ImplTiming))
  }

data StepVariantAggregate =
  StepVariantAgg
  { stepVariantid :: {-# UNPACK #-} !(Key Variant)
  , stepOptimalTime :: {-# UNPACK #-} !Double
  , stepImplTimes :: {-# UNPACK #-} !(Vector ImplTiming)
  }

stepAggregator
    :: [RawPredictor] -> ConduitT StepInfo VariantAggregate (Region SqlM) ()
stepAggregator predictors = do
    StepInfo{stepProps,stepTimings} <- C.peek >>= \case
        Just info -> return info
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one step input"

    genericPredictors <- V.fromList <$>
        mapM (cookPredictor stepProps stepTimings) predictors

    let zerooutPredictor :: CookedPredictor -> ImplTiming
        zerooutPredictor p = ImplTiming (predictedImplId - modelId) 0
          where
            modelId = fromIntegral . fromSqlKey $ predictorId p

        zerooutTiming :: ImplTiming -> ImplTiming
        zerooutTiming implTime = implTime { implTimingTiming = 0 }

        zeroTimeVec :: Vector ImplTiming
        zeroTimeVec = VS.convert (V.map zerooutPredictor genericPredictors)
                   <> VS.map zerooutTiming stepTimings

    foldGroup ((==) `on` stepVariantId) $
        aggregateSteps genericPredictors zeroTimeVec

aggregateSteps
    :: V.Vector CookedPredictor
    -> Vector ImplTiming
    -> ConduitT StepInfo VariantAggregate (Region SqlM) VariantAggregate
aggregateSteps predictors zeroTimeVec = do
    variantId <- C.peek >>= \case
        Just StepInfo{stepVariantId} -> return stepVariantId
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one step input"

    let initial :: (Maybe (Vector Int), StepVariantAggregate)
        initial = (Nothing, StepVariantAgg
            { stepVariantid = variantId
            , stepOptimalTime = 0
            , stepImplTimes = zeroTimeVec
            })

    toVariantAggregate . snd <$> C.foldl aggregate initial
  where
    toVariantAggregate :: StepVariantAggregate -> VariantAggregate
    toVariantAggregate StepVariantAgg{..} = VariantAgg
        { variantid = stepVariantid
        , optimalTime = stepOptimalTime
        , implTimes = Pair stepImplTimes VS.empty
        }

    aggregate
        :: (Maybe (Vector Int), StepVariantAggregate)
        -> StepInfo
        -> (Maybe (Vector Int), StepVariantAggregate)
    aggregate (!lastImpls, !StepVariantAgg{..}) !StepInfo{..} =
      (Just predictedImpls, StepVariantAgg
            { stepVariantid = stepVariantId
            , stepOptimalTime =
                stepOptimalTime + getTime (fromIntegral stepBestIdx)
            , stepImplTimes = VS.zipWith (liftImplTiming (+)) stepImplTimes
                    (newPredictions <> stepTimings)
            })
      where
        newPredictions :: Vector ImplTiming
        newPredictions = VS.imap wrapImpl predictedImpls
          where
            wrapImpl :: Int -> Int -> ImplTiming
            wrapImpl idx n
                | n == -1 = ImplTiming implId (0/0)
                | otherwise = ImplTiming implId (getTime n)
              where
                modelId = predictorId $ V.unsafeIndex predictors idx
                implId = predictedImplId - fromIntegral (fromSqlKey modelId)

        predictedImpls :: Vector Int
        predictedImpls = VS.generate (V.length predictors) doPredict
          where
            doPredict n = predictCooked (V.unsafeIndex predictors n) stepProps
                ((`VS.unsafeIndex` n) <$> lastImpls)

        getTime :: Int -> Double
        getTime ix = implTimingTiming $ stepTimings `VS.unsafeIndex` ix
