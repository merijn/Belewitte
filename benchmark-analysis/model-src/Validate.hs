{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Validate (validateModel) where

import Data.Bifunctor (first)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)

import Core
import Evaluate (percent)
import FormattedOutput (renderOutput)
import Model
import Query
import Schema
import StepQuery (StepInfo(..))
import Train

validateModel
    :: Key Algorithm -> Key Platform -> Model -> TrainingConfig -> SqlM ()
validateModel algoId platformId model config = renderOutput $ do
    validation <- getValidationQuery algoId platformId config
    computeResults "validation" $ reduceInfo <$> validation
    C.yield "\n"
    computeResults "total" $ reduceInfo <$> total
  where
    reduceInfo :: StepInfo -> (Vector Double, Int64)
    reduceInfo StepInfo{..} = (stepProps, stepBestImpl)

    total :: Query StepInfo
    total = getTotalQuery algoId platformId config

    computeResults
        :: Text -> Query (Vector Double, Int64) -> ConduitT () Text SqlM ()
    computeResults name query = do
        result <- runSqlQuery predictions .| C.foldl aggregate (0,0,0)
        C.yield $ report name result
      where
        predictions = first (predict model) <$> query

    aggregate :: (Int, Int, Int) -> (Int, Int64) -> (Int, Int, Int)
    aggregate (!right,!wrong,!unknown) (prediction,actual)
        | fromIntegral prediction == actual = (right + 1, wrong, unknown)
        | prediction == -1 = (right, wrong, unknown + 1)
        | otherwise = (right, wrong + 1, unknown)

report :: Text -> (Int, Int, Int) -> Text
report name (right, wrong, unknown) = T.unlines $
    [ "Right predictions (" <> name <> "): " <> showText right
    , "Wrong predictions (" <> name <> "): " <> showText wrong
    , "Unknown predictions (" <> name <> "): " <> showText unknown
    , "Error rate (" <> name <> "): " <> percent wrong (right+wrong+unknown)
    ]
