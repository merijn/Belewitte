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
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import Predictor (Predictor, predict)
import Schema
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
    :: (MonadLogger m, MonadThrow m)
    => Predictor -> ConduitT StepInfo Void m VariantAggregate
aggregateSteps predictor = do
    StepInfo{stepVariantId,stepTimings} <- C.peek >>= \case
        Just info -> return info
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one step input"

    let zerooutTiming :: ImplTiming -> ImplTiming
        zerooutTiming implTime = implTime { implTimingTiming = 0 }

        zeroTimeVec :: Vector ImplTiming
        zeroTimeVec = VS.map zerooutTiming stepTimings `VS.snoc`
            ImplTiming predictedImplId 0

        initial :: (Maybe Int, StepVariantAggregate)
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

    toVariantAggregate . snd <$> C.foldl (aggregate translateMap) initial
  where
    toVariantAggregate :: StepVariantAggregate -> VariantAggregate
    toVariantAggregate StepVariantAgg{..} = VariantAgg
        { variantid = stepVariantid
        , optimalTime = stepOptimalTime
        , implTimes = Pair stepImplTimes VS.empty
        }

    aggregate
        :: IntMap Int
        -> (Maybe Int, StepVariantAggregate)
        -> StepInfo
        -> (Maybe Int, StepVariantAggregate)
    aggregate translateMap (!lastImpl, !StepVariantAgg{..}) !StepInfo{..} =
      (Just predictedImpl, StepVariantAgg
            { stepVariantid = stepVariantId
            , stepOptimalTime = stepOptimalTime + getTime stepBestImpl
            , stepImplTimes = VS.zipWith (liftImplTiming (+)) stepImplTimes
                    (stepTimings `VS.snoc` newPrediction)
            })
      where
        newPrediction :: ImplTiming
        newPrediction = ImplTiming predictedImplId (getTime predictedImpl)

        predictedImpl :: Int
        predictedImpl = predict predictor stepProps lastImpl

        getTime :: Integral n => n -> Double
        getTime ix = implTimingTiming $
            stepTimings `VS.unsafeIndex` (translateMap IM.! fromIntegral ix)
