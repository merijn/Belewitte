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
import qualified Sql
import Utils.ImplTiming (ImplTiming(..))

data PredictionConfig = PredictionConfig
    { predConfigPredictorConfig :: PredictorConfig
    , predConfigStepInfoConfig :: StepInfoConfig
    , predConfigVariantId :: Key Variant
    }

predictSteps
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => RawPredictor -> ConduitT StepInfo (StepInfo, ImplTiming) m ()
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
        -> (Maybe Int, (StepInfo, ImplTiming))
    predictStep predictor step@StepInfo{..} lastPred = result
      where
        result = (Just newImpl, (step, newPrediction))

        predictedIdx = predictCooked predictor stepProps lastPred

        newPrediction = stepTimings `VS.unsafeIndex` predictedIdx
        newImpl = fromIntegral $ implTimingImpl newPrediction

formatStep
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => (StepInfo, ImplTiming) -> m Text
formatStep (StepInfo{..}, (ImplTiming predImplId predTime)) = do
    Entity _ predImpl@Implementation{} <-
        Sql.validateEntity "Implementation" predImplId

    return $ mconcat
        [ "Variant #", showSqlKey stepVariantId, " step #", showText stepId
        , ": ", showText predImplId, "\t", implementationName predImpl, "\t("
        , showText predTime, ")\n"
        ]

outputPredictorResults :: PredictionConfig -> SqlM ()
outputPredictorResults PredictionConfig{..} = do
    rawPredictor <- loadPredictor predConfigPredictorConfig
    renderRegionOutput $
        streamQuery stepQuery
        .| predictSteps rawPredictor
        .| C.mapM formatStep
  where
    stepQuery = stepInfoQuery predConfigStepInfoConfig predConfigVariantId
