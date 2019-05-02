{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Validate (validateModel) where

import Data.Bifunctor (first)
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Core
import Evaluate (percent)
import Model
import Query
import Schema
import StepQuery (StepInfo(..))
import Train

validateModel
    :: Key Algorithm -> Key Platform -> Model -> TrainingConfig -> SqlM ()
validateModel algoId platformId model config = do
    validation <- getValidationQuery algoId platformId config
    computeResults "validation" $ reduceInfo <$> validation
    computeResults "total" $ reduceInfo <$> total
  where
    reduceInfo StepInfo{..} = (stepProps, stepBestImpl)

    total = getTotalQuery algoId platformId config

    computeResults name query = do
        result <- runSqlQuery predictions $ C.foldl aggregate (0,0,0)
        report name result
      where
        predictions = first (predict model) <$> query

    aggregate (!right,!wrong,!unknown) (prediction,actual)
        | fromIntegral prediction == actual = (right + 1, wrong, unknown)
        | prediction == -1 = (right, wrong, unknown + 1)
        | otherwise = (right, wrong + 1, unknown)

report :: MonadIO m => Text -> (Int, Int, Int) -> m ()
report name (right, wrong, unknown) = liftIO . T.putStrLn . T.unlines $
    [ "Right predictions (" <> name <> "): " <> showText right
    , "Wrong predictions (" <> name <> "): " <> showText wrong
    , "Unknown predictions (" <> name <> "): " <> showText unknown
    , "Error rate (" <> name <> "): " <> percent wrong (right+wrong+unknown)
    ]
