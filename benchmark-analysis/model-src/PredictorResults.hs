{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PredictorResults (PredictionConfig(..), outputPredictorResults) where

import Control.Monad (void)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as C (mapAccum)
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS

import Core
import FormattedOutput (renderRegionOutput)
import Predictor
import Query (streamQuery)
import Query.Step
import Schema
import Sql (MonadSql)
import Utils.ImplTiming (ImplTiming(..))

data PredictionConfig = PredictionConfig
    { predConfigPredictorConfig :: PredictorConfig
    , predConfigStepInfoConfig :: StepInfoConfig
    , predConfigVariantId :: Key Variant
    }

predictSteps
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => RawPredictor -> ConduitT StepInfo (ImplTiming, StepInfo) m ()
predictSteps rawPredictor = do
    StepInfo{stepProps,stepTimings} <- C.peek >>= \case
        Just info -> return info
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one step input"

    predictor <- cookPredictor stepProps stepTimings rawPredictor

    void $ C.mapAccum (predictStep predictor) Nothing
  where
    predictStep
        :: CookedPredictor
        -> StepInfo
        -> Maybe Int
        -> (Maybe Int, (ImplTiming, StepInfo))
    predictStep predictor step@StepInfo{..} lastPred = result
      where
        result = (Just newImpl, (newPrediction, step))

        predictedIdx = predictCooked predictor stepProps lastPred

        newPrediction = stepTimings `VS.unsafeIndex` predictedIdx
        newImpl = fromIntegral $ implTimingImpl newPrediction

outputPredictorResults :: PredictionConfig -> SqlM ()
outputPredictorResults PredictionConfig{..} = do
    rawPredictor <- loadPredictor predConfigPredictorConfig
    renderRegionOutput $
        streamQuery stepQuery
        .| predictSteps rawPredictor
        .| C.map (T.pack . show)
  where
    stepQuery = stepInfoQuery predConfigStepInfoConfig predConfigVariantId
