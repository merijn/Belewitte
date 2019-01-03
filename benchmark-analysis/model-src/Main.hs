{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main(main) where

import Control.Monad (forM_, void)
import Data.Bifunctor (first)
import Data.Conduit (ConduitT, Void, (.|), await)
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.Combinators as C
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Core
import Evaluate (evaluateModel, compareImplementations, percent)
import Model
import Options
import Query
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

querySink
    :: (MonadResource m, MonadThrow m, Show a)
    => Maybe String
    -> FilePath
    -> ConduitT a Void m ()
querySink Nothing _ = void await
querySink (Just suffix) name =
    C.map showText
    .| C.map (`T.snoc` '\n')
    .| C.encode C.utf8
    .| C.sinkFile (name <> suffix)

main :: IO ()
main = runSqlM commands $ \case
    Train{getAlgoId,getGpuId,getConfig} -> do
        algoId <- getAlgoId
        gpuId <- getGpuId
        trainConfig <- getConfig
        modelId <- fst <$> trainModel algoId gpuId trainConfig
        liftIO $ print (fromSqlKey modelId)

    Query{getModel} -> do
        modelId <- fst <$> getModel
        getModelStats modelId >>= reportModelStats

    Validate{getAlgoId,getGpuId,getModel} -> do
        algoId <- getAlgoId
        gpuId <- getGpuId
        (modelId, model) <- getModel
        trainConfig <- getModelTrainingConfig modelId
        validateModel algoId gpuId model trainConfig

    Evaluate{getAlgoId,getGpuId,getModel,defaultImpl,reportConfig} -> void $ do
        algoId <- getAlgoId
        gpuId <- getGpuId
        (modelId, model) <- getModel
        trainConfig <- getModelTrainingConfig modelId
        evaluateModel algoId gpuId defaultImpl reportConfig model trainConfig

    Compare{getAlgoId,getGpuId,reportConfig} -> void $ do
        algoId <- getAlgoId
        gpuId <- getGpuId
        compareImplementations algoId gpuId reportConfig

    Export{getAlgoId,getModel,cppFile} -> void $ do
        algoId <- getAlgoId
        impls <- queryImplementations algoId
        (modelId, model) <- getModel
        TrainConfig{..} <- getModelTrainingConfig modelId
        dumpCppModel cppFile model trainGraphProps trainStepProps
            (implementationName <$> impls)

    QueryTest{getAlgoId,getGpuId,outputSuffix} -> do
        algoId <- getAlgoId
        gpuId <- getGpuId

        graphPropQuery <- getDistinctFieldQuery GraphPropProperty
        stepPropQuery <- getDistinctFieldQuery StepPropProperty

        graphprops <- runSqlQuery graphPropQuery $ C.foldMap S.singleton
        stepprops <- runSqlQuery stepPropQuery $ C.foldMap S.singleton

        let stepQuery :: Query StepInfo
            stepQuery = stepInfoQuery algoId gpuId graphprops stepprops

            variantQuery :: Query VariantInfo
            variantQuery = variantInfoQuery algoId gpuId

        runSqlQuery stepQuery $ querySink outputSuffix "stepInfoQuery-"
        runSqlQuery variantQuery $ querySink outputSuffix "variantInfoQuery-"
