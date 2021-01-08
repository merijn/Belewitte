{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PredictorResults (PredictionConfig(..), outputPredictorResults) where

import Control.Monad (void, when)
import Data.Conduit (ConduitT, (.|), yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as C (mapAccum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector.Storable (Vector)
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
import Utils.PropValue (PropValue(..))

data PredictionConfig = PredictionConfig
    { predConfigPredictorConfig :: PredictorConfig
    , predConfigStepInfoConfig :: StepInfoConfig
    , predConfigVariantId :: Key Variant
    , predConfigShowProps :: Bool
    }

renderProps
    :: Text
    -> Map Int64 PropertyName
    -> Maybe Int64
    -> Vector PropValue
    -> Text
renderProps propType lut mStepId =
    VS.foldl' (\prev prop -> prev <> renderProp prop) ""
  where
    stepLabel = case mStepId of
        Nothing -> ""
        Just i -> showText i <> ":"

    renderProp :: PropValue -> Text
    renderProp PropValue{..} = case M.lookup propValuePropId lut of
        Nothing -> ""
        Just PropertyName{propertyNameProperty} -> mconcat
            [ propType, ":", stepLabel, propertyNameProperty
            , ":", showText propValueValue, "\n" ]

predictSteps
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Bool -> RawPredictor -> ConduitT StepInfo Text m ()
predictSteps showProps rawPredictor = do
    StepInfo{stepProps,stepTimings} <- C.peek >>= \case
        Just info -> return info
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one step input"

    (stepPropMap, graphPropMap) <- case showProps of
        False -> return (M.empty, M.empty)
        True -> M.partition propertyNameIsStepProp <$>
                    VS.foldM' buildLookup M.empty stepProps

    yield $ renderProps "graph" graphPropMap Nothing stepProps

    predictor <- cookPredictor stepProps stepTimings rawPredictor

    void (C.mapAccum (predictStep predictor) Nothing)
        .| C.mapM (formatStep stepPropMap)
  where
    buildLookup
        :: (MonadLogger m, MonadSql m, MonadThrow m)
        => Map Int64 PropertyName
        -> PropValue
        -> m (Map Int64 PropertyName)
    buildLookup lut PropValue{..} = do
        val <- Sql.getJust $ toSqlKey propValuePropId

        when (propValuePropId `M.member` lut) . logThrowM $
            GenericInvariantViolation "Found duplicate property id in input!"

        return $ M.insert propValuePropId val lut

    predictStep
        :: CookedPredictor
        -> StepInfo
        -> Maybe Index
        -> (Maybe Index, (StepInfo, ImplTiming))
    predictStep predictor step@StepInfo{..} lastPred = result
      where
        result = (Just predictedIdx, (step, newPrediction))

        predictedIdx = predictCooked predictor stepProps lastPred

        newPrediction = stepTimings `VS.unsafeIndex` getIdx predictedIdx

formatStep
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Map Int64 PropertyName -> (StepInfo, ImplTiming) -> m Text
formatStep propMap (StepInfo{..}, (ImplTiming predImplId predTime)) = do
    Entity _ predImpl@Implementation{} <- Sql.validateEntity predImplId

    return $ mconcat
        [ renderProps "step" propMap (Just stepId) stepProps
        , "Variant #", showSqlKey stepVariantId, " step #", showText stepId
        , ": ", showText predImplId, "\t", implementationName predImpl, "\t("
        , showText predTime, ")\n"
        ]

outputPredictorResults :: PredictionConfig -> SqlM ()
outputPredictorResults PredictionConfig{..} = do
    rawPredictor <- loadPredictor predConfigPredictorConfig
    renderRegionOutput $
        streamQuery stepQuery
        .| predictSteps predConfigShowProps rawPredictor
  where
    stepQuery = stepInfoQuery predConfigStepInfoConfig predConfigVariantId
