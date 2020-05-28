{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module StepAggregate (VariantAggregate(..), aggregateSteps) where

import Data.Conduit (ConduitT, Void)
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import Predictor
import Schema
import Sql (MonadSql)
import StepQuery (StepInfo(..))
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

aggregateSteps
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => [Predictor] -> ConduitT StepInfo Void m VariantAggregate
aggregateSteps predictors = do
    StepInfo{stepVariantId,stepProps,stepTimings} <- C.peek >>= \case
        Just info -> return info
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one step input"

    gPredictors <- V.fromList <$> mapM (makeGeneralPredictor stepProps) predictors

    let zerooutTiming :: ImplTiming -> ImplTiming
        zerooutTiming implTime = implTime { implTimingTiming = 0 }

        zeroTimeVec :: Vector ImplTiming
        zeroTimeVec = VS.map zerooutTiming stepTimings <>
            VS.replicate (length predictors) (ImplTiming predictedImplId 0)

        initial :: (Maybe (Vector Int), StepVariantAggregate)
        initial = (Nothing, StepVariantAgg
            { stepVariantid = stepVariantId
            , stepOptimalTime = 0
            , stepImplTimes = zeroTimeVec
            })

        translateMap :: IntMap Int
        translateMap = VS.ifoldl' build IM.empty zeroTimeVec
          where
            build :: IntMap Int -> Int -> ImplTiming -> IntMap Int
            build implMap idx (ImplTiming impl _) =
                IM.insert (fromIntegral impl) idx implMap

    toVariantAggregate . snd <$>
        C.foldl (aggregate gPredictors translateMap) initial
  where
    toVariantAggregate :: StepVariantAggregate -> VariantAggregate
    toVariantAggregate StepVariantAgg{..} = VariantAgg
        { variantid = stepVariantid
        , optimalTime = stepOptimalTime
        , implTimes = Pair stepImplTimes VS.empty
        }

    aggregate
        :: V.Vector GeneralPredictor
        -> IntMap Int
        -> (Maybe (Vector Int), StepVariantAggregate)
        -> StepInfo
        -> (Maybe (Vector Int), StepVariantAggregate)
    aggregate ps translateMap (!lastImpls, !StepVariantAgg{..}) !StepInfo{..} =
      (Just predictedImpls, StepVariantAgg
            { stepVariantid = stepVariantId
            , stepOptimalTime = stepOptimalTime + getTime stepBestImpl
            , stepImplTimes = VS.zipWith (liftImplTiming (+)) stepImplTimes
                    (stepTimings <> newPredictions)
            })
      where
        newPredictions :: Vector ImplTiming
        newPredictions = VS.map wrapImpl predictedImpls
          where
            wrapImpl :: Int -> ImplTiming
            wrapImpl n
                | n == -1 = ImplTiming predictedImplId (0/0)
                | otherwise = ImplTiming predictedImplId (getTime n)

        predictedImpls :: Vector Int
        predictedImpls = VS.generate (V.length ps) doPredict
          where
            doPredict n = predictGeneral (V.unsafeIndex ps n) stepProps
                ((`VS.unsafeIndex` n) <$> lastImpls)

        getTime :: Integral n => n -> Double
        getTime ix = implTimingTiming $
            stepTimings `VS.unsafeIndex` (translateMap IM.! fromIntegral ix)
