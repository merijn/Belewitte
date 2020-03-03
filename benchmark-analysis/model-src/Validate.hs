{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Validate (validateModel) where

import Data.Bifunctor (first)
import Data.Conduit (ConduitT)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Data.Vector.Storable (Vector)

import Core
import FormattedOutput (renderOutput)
import Model.Stats (implInUnknownSet)
import Predictor
import Query
import Schema
import StepQuery (StepInfo(..))
import Train

data ValidateStats = ValidateStats
    { rightPreds :: !Int
    , partialPreds :: !Int
    , wrongPreds :: !Int
    , unknownPreds :: !Int
    } deriving (Show)

validateModel :: Predictor -> TrainingConfig -> SqlM ()
validateModel predictor config = renderOutput $ do
    validation <- getValidationQuery config
    computeResults "validation" $ reduceInfo <$> validation
    C.yield "\n"
    computeResults "total" $ reduceInfo <$> total
  where
    reduceInfo :: StepInfo -> (Vector Double, Int64)
    reduceInfo StepInfo{..} = (stepProps, stepBestImpl)

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
