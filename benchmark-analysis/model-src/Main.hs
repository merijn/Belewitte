{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main(main) where

import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Text.IO as T

import Core
import Evaluate (evaluateModel, compareImplementations, percent)
import Model
import Options
import Schema
import Train
import Validate

reportModelStats :: MonadIO m => ModelStats -> m ()
reportModelStats ModelStats{..} = liftIO $ do
  forM_ sortedFeatures $ \(lbl, val) -> do
    T.putStrLn $ lbl <> ": " <> percent val 1

  forM_ sortedUnknown $ \(count, implSet) -> do
      T.putStrLn $ percent count modelUnknownCount <> " : " <> showText implSet
  where
    features :: [(Text, Double)]
    features = map (first ("Graph:" <>)) (M.toList modelGraphPropImportance)
            ++ map (first ("Step:" <>)) (M.toList modelStepPropImportance)

    sortedFeatures :: [(Text, Double)]
    sortedFeatures = sortBy (flip (comparing snd)) features

    sortedUnknown :: [(Int, Set Int64)]
    sortedUnknown = sortBy (flip (comparing fst)) modelUnknownPreds

main :: IO ()
main = runSqlM commands $ \case
    Train{getAlgoId,getPlatformId,getConfig} -> do
        algoId <- getAlgoId
        platformId <- getPlatformId
        trainConfig <- getConfig
        modelId <- fst <$> trainModel algoId platformId trainConfig
        liftIO $ print (fromSqlKey modelId)

    Query{getModel} -> do
        modelId <- fst <$> getModel
        getModelStats modelId >>= reportModelStats

    Validate{getAlgoId,getPlatformId,getModel} -> do
        algoId <- getAlgoId
        platformId <- getPlatformId
        (modelId, model) <- getModel
        trainConfig <- getModelTrainingConfig modelId
        validateModel algoId platformId model trainConfig

    Evaluate{getAlgoId,getPlatformId,getModel,defaultImpl,reportConfig} -> do
        algoId <- getAlgoId
        platId <- getPlatformId
        (modelId, model) <- getModel
        trainConfig <- getModelTrainingConfig modelId
        evaluateModel algoId platId defaultImpl reportConfig model trainConfig

    Compare{getAlgoId,getPlatformId,reportConfig} -> do
        algoId <- getAlgoId
        platformId <- getPlatformId
        compareImplementations algoId platformId reportConfig

    Export{getAlgoId,getModel,cppFile} -> do
        algoId <- getAlgoId
        impls <- queryImplementations algoId
        (modelId, model) <- getModel
        TrainConfig{..} <- getModelTrainingConfig modelId
        dumpCppModel cppFile model trainGraphProps trainStepProps
            (implementationName <$> impls)
