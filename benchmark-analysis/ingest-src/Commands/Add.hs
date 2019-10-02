{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Commands.Add (commands) where

import Control.Monad (mzero, unless, when)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Conduit (ConduitT, Void, (.|), runConduit)
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath (splitExtension, takeFileName)

import Core
import InteractiveInput
import OptionParsers
import Schema
import Sql (Entity(..), MonadSql, SqlRecord, (=.), (==.))
import qualified Sql

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
    => Key rec -> EntityField rec Bool -> Text -> Input SqlM ()
checkSetDefault key field prompt = do
    n <- Sql.count [field ==. True]
    when (n == 0) $ do
        isDefault <- getInteractive readInput prompt
        when isDefault $ Sql.update key [field =. True]

addPlatform :: Input SqlM ()
addPlatform = do
    platformName <- getInteractive textInput "Platform Slurm Name"
    prettyName <- getInteractive optionalInput "Platform Pretty Name"
    platformId <- Sql.insert $ Platform platformName prettyName False
    checkSetDefault platformId PlatformIsDefault defaultPrompt
  where
    defaultPrompt = "Default platform for computing properties"

addGraphs :: [FilePath] -> Input SqlM ()
addGraphs paths = do
    datasetTag <- getInteractive textInput "Dataset Tag"
    datasetId <- Sql.insert $ Dataset datasetTag

    runConduit $
        C.yieldMany paths
        .| C.concatMapM (lift . filterGraphPaths)
        .| C.mapM (insertGraph datasetId)
        .> insertVariants
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
        :: MonadSql m => Key Dataset -> (String, String) -> m (Key Graph)
    insertGraph datasetId (graphName, path) =
        Sql.insert $ Graph (T.pack graphName) (T.pack path) Nothing datasetId

    insertVariants
        :: (MonadResource m, MonadSql m)
        => Key Graph -> ConduitT (Key Graph) Void m ()
    insertVariants graphId = Sql.selectSource [] [] .| C.mapM_ insertVariant
      where
        insertVariant :: MonadSql m => Entity VariantConfig -> m ()
        insertVariant (Entity variantCfgId (VariantConfig algoId _ _ _)) =
            Sql.insert_ $ Variant graphId variantCfgId algoId Nothing False 0

addAlgorithm :: Input SqlM ()
addAlgorithm = do
    algoName <- getInteractive input "Algorithm Name"
    prettyName <- getInteractive optionalInput "Algorithm Pretty Name"
    Sql.insert_ $ Algorithm algoName prettyName
  where
    input = processCompleterText ["list","algorithms"]

addImplementation :: Input SqlM ()
addImplementation = do
    Entity algoId Algorithm{..} <- getInteractive algoInput "Algorithm Name"
    implName <- getInteractive (implInput algorithmName) "Implementation Name"

    prettyName <- getInteractive optionalInput "Implementation Pretty Name"
    flags <- getInteractive optionalInput "Flags"
    implType <- getInteractive typeInput "Implementation type"

    Sql.insert_ $ Implementation algoId implName prettyName flags implType
  where
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    implInput algorithmName = processCompleterText
        ["list","implementations","-a",T.unpack algorithmName]
    typeInput = readInputEnsure (/=Builtin)

addVariant :: Input SqlM ()
addVariant = do
    Entity algoId _ <- getInteractive algoInput "Algorithm Name"
    variantName <- getInteractive textInput "Variant Name"
    flags <- getInteractive optionalInput "Flags"
    varCfgId <- Sql.insert $ VariantConfig algoId variantName flags False

    checkSetDefault varCfgId VariantConfigIsDefault defaultPrompt

    let mkVariant gId = Variant gId varCfgId algoId Nothing False 0

    runConduit $ Sql.selectKeys [] [] .| C.mapM_ (Sql.insert_ . mkVariant)
  where
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    defaultPrompt = "Default Variant (for plotting)"

addRunConfig :: Input SqlM ()
addRunConfig = do
    Entity algoId Algorithm{..} <- getInteractive algoInput "Algorithm Name"
    Entity platformId _ <- getInteractive platformInput "Platform Name"
    Entity datasetId _ <- getInteractive datasetInput "Dataset Name"
    version <- getInteractive (versionInput algorithmName) "Algorithm Version"
    repeats <- getInteractive (readInputEnsure (>0)) "Number of runs"

    Sql.insert_ $ RunConfig algoId platformId datasetId version repeats
  where
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    platformInput = sqlInput PlatformName UniqPlatform
    datasetInput = sqlInput DatasetName UniqDataset
    versionInput algorithmName = processCompleterText
        ["query","algorithm-version","-a",T.unpack algorithmName]

