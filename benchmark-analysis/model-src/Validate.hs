{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Validate (validateModel) where

import Data.Bifunctor (first)
import Data.Conduit (ConduitT)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Set (Set)
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import FormattedOutput (renderOutput)
import Model.Stats (implInUnknownSet)
import Predictor
import Query
import Schema
import StepQuery (StepInfo(..))
import Train
import Utils.PropValue (propValueValue)

data ValidateStats = ValidateStats
    { rightPreds :: !Int
    , partialPreds :: !Int
    , wrongPreds :: !Int
    , unknownPreds :: !Int
    } deriving (Show)

validateModel
    :: RawPredictor
    -> TrainingConfig
    -> Maybe (Key Platform)
    -> Maybe (Set (Key Dataset))
    -> SqlM ()
validateModel predictor config mPlatform mDatasets = renderOutput $
    case (mPlatform, mDatasets) of
        (Nothing, Nothing) -> do
            validation <- getValidationQuery config
            computeResults "validation" $ reduceInfo <$> validation
            C.yield "\n"
            computeResults "total" $ reduceInfo <$> total
        _ -> do
            let setPlatform = maybe id setTrainingConfigPlatform mPlatform
                setDatasets = setTrainingConfigDatasets mDatasets

                newConfig = setPlatform . setDatasets $ config
                comparisonQuery = getTotalQuery newConfig

            computeResults "comparison" $ reduceInfo <$> comparisonQuery
  where
    reduceInfo :: StepInfo -> (Vector Double, Int64)
    reduceInfo StepInfo{..} = (VS.map propValueValue stepProps, stepBestImpl)

    total :: Query StepInfo
    total = getTotalQuery config

    computeResults
        :: Text -> Query (Vector Double, Int64) -> ConduitT () Text SqlM ()
    computeResults name query = do
        result <- runSqlQueryConduit predictions $ C.foldl aggregate initial
        C.yield $ report name result
      where
        initial = ValidateStats 0 0 0 0
        predictions = first (rawPredict predictor) <$> query

    aggregate :: ValidateStats -> (RawPrediction, Int64) -> ValidateStats
    aggregate stats@ValidateStats{..} (prediction,actual) = case prediction of
        ImplPrediction (fromIntegral -> impl)
            | impl == actual -> stats{ rightPreds = 1 + rightPreds }
            | otherwise -> stats{ wrongPreds = 1 + wrongPreds }

        MisPredictionSet unknowns
            | actual `implInUnknownSet` unknowns ->
                    stats{ partialPreds = 1 + partialPreds }
            | otherwise -> stats{ wrongPreds = 1 + wrongPreds }

        Unknown -> stats{ unknownPreds = 1 + unknownPreds }

report :: Text -> ValidateStats -> Text
report name ValidateStats{..} = T.unlines $
    [ "Right predictions (" <> name <> "): " <> showText rightPreds
    , "Partial predictions (" <> name <> "): " <> showText partialPreds
    , "Wrong predictions (" <> name <> "): " <> showText wrongPreds
    , "Unknown predictions (" <> name <> "): " <> showText unknownPreds
    , "Soft error rate (" <> name <> "): " <> percent wrongPreds totalPreds
    , "Hard error rate (" <> name <> "): "
        <> percent (wrongPreds + partialPreds) totalPreds
    ]
  where
    totalPreds = rightPreds + partialPreds + wrongPreds + unknownPreds
