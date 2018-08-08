{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad (forM_, void)
import Data.Bifunctor (first)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import Data.Set (Set)
import Data.String (fromString)
import qualified Data.Text.IO as T
import Database.Persist.Sqlite (Entity(..), (==.))
import qualified Database.Persist.Sqlite as Sql
import GHC.Conc.Sync (getNumProcessors, setNumCapabilities)
import System.Environment (getProgName)

import Evaluate (evaluateModel, compareImplementations, percent)
import Model
import Options
import Query
import Schema
import Train
import Validate

queryImplementations :: SqlM (IntMap Implementation)
queryImplementations = do
    Just (Entity aId _) <- selectBfs
    runConduitRes $ selectImpls aId .| C.foldMap toIntMap
  where
    selectBfs = Sql.selectFirst [ AlgorithmName ==. "bfs" ] []

    selectImpls aId = Sql.selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

dumpQueryToFile :: Show r => String -> Query r -> SqlM ()
dumpQueryToFile path query = runSqlQuery query $
    C.map ((++"\n") . show) .| C.map fromString .| C.sinkFile path

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
main = do
    getNumProcessors >>= setNumCapabilities
    Options{..} <- getProgName >>= optionsParser

    runSqlM logVerbosity database $ do
        Sql.liftPersist $ Sql.runMigrationSilent migrateAll

        impls <- queryImplementations
        case modelTask of
            Train{getGpuId,getConfig} -> do
                gpuId <- getGpuId
                trainConfig <- getConfig
                modelId <- fst <$> trainModel gpuId trainConfig
                liftIO $ print (fromSqlKey modelId)

            Query{getModel} -> do
                modelId <- fst <$> getModel
                getModelStats modelId >>= reportModelStats

            Validate{getGpuId,getModel} -> do
                gpuId <- getGpuId
                (modelId, model) <- getModel
                trainConfig <- getModelTrainingConfig modelId
                validateModel gpuId model trainConfig

            Evaluate{getGpuId,getModel,reportConfig} -> void $ do
                gpuId <- getGpuId
                (modelId, model) <- getModel
                trainConfig <- getModelTrainingConfig modelId
                evaluateModel gpuId reportConfig model trainConfig impls

            Compare{getGpuId,reportConfig} -> void $ do
                gpuId <- getGpuId
                compareImplementations gpuId reportConfig impls

            Export{getModel,cppFile} -> void $ do
                (modelId, model) <- getModel
                TrainConfig{..} <- getModelTrainingConfig modelId
                dumpCppModel cppFile model trainGraphProps trainStepProps impls
