{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Commands.Add (commands) where

import Control.Monad (mzero, unless, when)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (doesFileExist)
import System.FilePath (splitExtension, takeFileName)

import Core
import InteractiveInput
import Options
import Schema
import Sql (Filter, MonadSql, SqlRecord, Transaction, (=.), (==.))
import qualified Sql
import qualified Sql.Transaction as SqlTrans

commands :: Command (Input SqlM ())
commands = CommandGroup CommandInfo
    { commandName = "add"
    , commandHeaderDesc = "add new entries to the database"
    , commandDesc = "Used to register setups for experiments"
    }
    [ SingleCommand CommandInfo
        { commandName = "platform"
        , commandHeaderDesc = "register a new platform"
        , commandDesc = "Register a new platform in the database"
        }
        $ pure addPlatform
    , SingleCommand CommandInfo
        { commandName = "graphs"
        , commandHeaderDesc = "register graphs"
        , commandDesc = "Register new graphs in the database"
        }
        $ addGraphs <$> some (strArgument graphFile)
    , SingleCommand CommandInfo
        { commandName = "algorithm"
        , commandHeaderDesc = "register a new algorithm"
        , commandDesc = "Register a new algorithm in the database"
        }
        $ pure addAlgorithm
    , SingleCommand CommandInfo
        { commandName = "implementation"
        , commandHeaderDesc = "register a new implementation"
        , commandDesc = "Register a new implementation of an algorithm"
        }
        $ pure addImplementation
    , SingleCommand CommandInfo
        { commandName = "variant"
        , commandHeaderDesc = "register a new variant"
        , commandDesc = "Register a new variant of an algorithm for a graph"
        }
        $ pure addVariant
    , SingleCommand CommandInfo
        { commandName = "run-config"
        , commandHeaderDesc = "register a new run configuration"
        , commandDesc = "Register a new configuration of benchmarks to run."
        }
        $ pure addRunConfig
    ]
  where
    graphFile = metavar "GRAPH" <> help "Graph file"

checkSetDefault
    :: (SqlRecord rec)
    => Key rec
    -> EntityField rec Checkmark
    -> Text
    -> [Filter rec]
    -> Transaction (Input SqlM) ()
checkSetDefault key field prompt filters = do
    n <- SqlTrans.count $ [field ==. Active] ++ filters
    when (n == 0) $ do
        isDefault <- lift $ getInteractive readInput prompt
        when isDefault $ SqlTrans.update key [field =. Active]

addPlatform :: Input SqlM ()
addPlatform = withInteractiveLogging $ do
    platformName <- getInteractive textInput "Platform (Slurm) Name"
    prettyName <- getInteractive optionalInput "Platform Pretty Name"
    runFlags <- getInteractive optionalInput "Platform Flags"
    platformCount <- getInteractive (readInputEnsure (>0)) "Available Machines"

    SqlTrans.runTransaction $ do
        platformId <- SqlTrans.insert $
            Platform platformName prettyName runFlags platformCount Inactive

        checkSetDefault platformId PlatformIsDefault defaultPrompt []
  where
    defaultPrompt = "Default platform for computing properties"

addGraphs :: [FilePath] -> Input SqlM ()
addGraphs paths = do
    datasetTag <- withInteractiveLogging $
        getInteractive textInput "Dataset Tag"

    ts <- liftIO getCurrentTime

    SqlTrans.runTransaction $ do
        datasetId <- SqlTrans.insert $ Dataset datasetTag

        runConduit $
            C.yieldMany paths
            .| C.concatMapM (lift . lift . filterGraphPaths)
            .| C.mapM (insertGraph ts datasetId)
            .| C.mapM_ insertVariants
  where
    filterGraphPaths
        :: (MonadIO m, MonadLogger m) => String -> m (Maybe (String, String))
    filterGraphPaths path = runMaybeT $ do
        unless (ext == ".graph") $ do
            logErrorN $ "Not a .graph file: " <> T.pack path
            mzero

        exists <- liftIO $ doesFileExist path
        unless exists $ do
            logErrorN $ "Graph file does not exist: " <> T.pack path
            mzero

        return (graphName, path)
      where
        (graphName, ext) = splitExtension $ takeFileName path

    insertGraph
        :: MonadSql m
        => UTCTime -> Key Dataset -> (String, String) -> Transaction m (Key Graph)
    insertGraph ts datasetId (graphName, path) = SqlTrans.insert $
        Graph (T.pack graphName) (T.pack path) Nothing datasetId ts

    insertVariants
        :: (MonadResource m, MonadSql m) => Key Graph -> Transaction m ()
    insertVariants graphId =
        SqlTrans.selectSource [] [] $ C.mapM_ insertVariant
      where
        insertVariant :: MonadSql m => Entity VariantConfig -> Transaction m ()
        insertVariant (Entity variantCfgId (VariantConfig algoId _ _ _ _)) =
            SqlTrans.insert_ $
                Variant graphId variantCfgId algoId Nothing 0 False 0

addAlgorithm :: Input SqlM ()
addAlgorithm = withInteractiveLogging $ do
    algoName <- getInteractive input "Algorithm Name"
    prettyName <- getInteractive optionalInput "Algorithm Pretty Name"
    Sql.insert_ $ Algorithm algoName prettyName
  where
    input = processCompleterText ["list","algorithms"]

addImplementation :: Input SqlM ()
addImplementation = withInteractiveLogging $ do
    Entity algoId Algorithm{..} <- getInteractive algoInput "Algorithm Name"
    implName <- getInteractive (implInput algorithmName) "Implementation Name"

    prettyName <- getInteractive optionalInput "Implementation Pretty Name"
    flags <- getInteractive optionalInput "Flags"
    implType <- getInteractive typeInput "Implementation type"

    ts <- liftIO getCurrentTime
    Sql.insert_ $ Implementation algoId implName prettyName flags implType ts
  where
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    implInput algorithmName = processCompleterText
        ["list","implementations","-a",T.unpack algorithmName]
    typeInput = readInputEnsure (/=Builtin)

addVariant :: Input SqlM ()
addVariant = do
    ts <- liftIO getCurrentTime

    (algoId, variantConfig) <- withInteractiveLogging $ do
        Entity algoId _ <- getInteractive algoInput "Algorithm Name"
        variantName <- getInteractive textInput "Variant Name"
        flags <- getInteractive optionalInput "Flags"
        return $ (algoId, VariantConfig algoId variantName flags Inactive ts)

    SqlTrans.runTransaction $ do
        varCfgId <- SqlTrans.insert $ variantConfig

        checkSetDefault varCfgId VariantConfigIsDefault defaultPrompt
            [VariantConfigAlgorithmId ==. algoId]

        let mkVariant gId = Variant gId varCfgId algoId Nothing 0 False 0

        SqlTrans.selectKeys [] [] $ C.mapM_ (SqlTrans.insert_ . mkVariant)
  where
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    defaultPrompt = "Default Variant (for plotting)"

addRunConfig :: Input SqlM ()
addRunConfig = withInteractiveLogging $ do
    Entity algoId Algorithm{..} <- getInteractive algoInput "Algorithm Name"
    Entity platformId _ <- getInteractive platformInput "Platform Name"
    Entity datasetId _ <- getInteractive datasetInput "Dataset Name"
    version <- getInteractive (versionInput algorithmName) "Algorithm Version"
    repeats <- getInteractive (readInputEnsure (>0)) "Number of runs"

    Sql.insert_ $
        RunConfig algoId platformId datasetId (CommitId version) repeats
  where
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    platformInput = sqlInput PlatformName UniqPlatform
    datasetInput = sqlInput DatasetName UniqDataset
    versionInput algorithmName = processCompleterText
        ["query","algorithm-version","-a",T.unpack algorithmName]

