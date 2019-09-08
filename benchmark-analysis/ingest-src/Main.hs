{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main(main) where

import Control.Monad (forM_, guard, void)
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (doesFileExist)
import System.FilePath (splitExtension, takeFileName)

import Core
import InteractiveInput
import Jobs
import MissingQuery (missingQuery)
import OptionParsers
import Parsers
import ProcessPool
import Query (runSqlQuery)
import qualified RuntimeData
import Schema
import Sql (Entity(..), Filter, MonadSql, (=.), (==.))
import qualified Sql

addPlatform :: Input SqlM ()
addPlatform = do
    platformName <- getInteractive textInput "Platform Slurm Name"
    prettyName <- getInteractive optionalInput "Platform Pretty Name"
    Sql.insert_ $ Platform platformName prettyName

addGraphs :: [FilePath] -> Input SqlM ()
addGraphs paths = do
    datasetTag <- getInteractive textInput "Dataset Tag"
    datasetId <- Sql.insert $ Dataset datasetTag
    forM_ paths $ \path -> do
        liftIO $ doesFileExist path >>= guard
        let (graphName, ext) = splitExtension $ takeFileName path
        liftIO $ guard (ext == ".graph")
        Sql.insert_ $ Graph (T.pack graphName) (T.pack path) Nothing datasetId

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
    implType <- getInteractive (readInput (/=Builtin)) "Implementation type"

    Sql.insert_ $ Implementation algoId implName prettyName flags implType
  where
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    implInput algorithmName = processCompleterText
        ["list","implementations","-a",T.unpack algorithmName]

addVariant :: Input SqlM ()
addVariant = do
    Entity algoId _ <- getInteractive algoInput "Algorithm Name"
    variantName <- getInteractive textInput "Variant Name"
    flags <- getInteractive optionalInput "Flags"

    let mkVariant gId = Variant gId algoId variantName flags Nothing False 0

    runConduit $ Sql.selectKeys [] [] .| C.mapM_ (Sql.insert_ . mkVariant)
  where
    algoInput = sqlInput AlgorithmName UniqAlgorithm

importResults :: Input SqlM ()
importResults = do
    Entity platformId _ <- getInteractive platformInput "Platform Name"
    Entity algoId _ <- getInteractive algoInput "Algorithm Name"
    Entity implId _ <- getInteractive (implInput algoId) "Implementation Name"

    filepath <- getInteractive filepathInput "Result File"

    timestamp <- liftIO getCurrentTime
    runConduit $
        C.sourceFile filepath
        .| C.decode C.utf8
        .| C.map (T.replace "," "")
        .| conduitParse externalResult
        .| C.mapM_ (insertResult platformId algoId implId timestamp)
  where
    platformInput = sqlInput PlatformName UniqPlatform
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    implInput algoId = sqlInput ExternalImplName (UniqExternalImpl algoId)

    insertResult
        :: (MonadSql m, MonadTagFail m)
        => Key Platform
        -> Key Algorithm
        -> Key ExternalImpl
        -> UTCTime
        -> ExternalResult
        -> m ()
    insertResult platId algoId implId ts (ExternalResult gname varName Timing{..}) = do
        [graphId] <- logIfFail "More than one graph found for" gname $
            Sql.selectKeysList [GraphName ==. gname] []

        let uniqVariant = UniqVariant graphId algoId variantName

        Just varId <- logIfFail "No variant found" variantName $
            fmap entityKey <$> Sql.getBy uniqVariant

        Sql.insert_ $
          ExternalTimer platId varId implId name minTime avgTime maxTime stddev ts
      where
        --FIXME get from command
        variantName
            | varName == "0" = "default"
            | otherwise = "Root " <> varName

runBenchmarks :: Int -> Input SqlM ()
runBenchmarks numNodes = lift $ do
    -- Error out if C++ code hasn't been compiled
    void $ do
        RuntimeData.getKernelExecutable
        RuntimeData.getKernelLibPath

    runConduit $
        Sql.selectSource [] []
        .> variantToPropertyJob
        -- FIXME hardcoded Platform
        .| processJobsParallel numNodes (Platform "TitanX" Nothing)
        .| C.mapM_ processProperty

    runConduit $
        Sql.selectSource [] [] .> \(Entity runConfigId config) -> do
            let commitId = runConfigAlgorithmVersion config
            platform <- Sql.getJust $ runConfigPlatformId config

            runSqlQuery (missingQuery runConfigId)
                .> missingRunToTimingJob
                .| processJobsParallel numNodes platform
                .| C.mapM_ (processTiming runConfigId commitId)

resetRetries :: MonadSql m => m ()
resetRetries = Sql.updateWhere [] [VariantRetryCount =. 0]

resetProperties :: MonadSql m => m ()
resetProperties = do
    Sql.deleteWhere ([] :: [Filter GraphProp])
    Sql.deleteWhere ([] :: [Filter StepProp])
    Sql.updateWhere [] [VariantPropsStored =. False]

resetMeasurements :: MonadSql m => m ()
resetMeasurements = do
    Sql.deleteWhere ([] :: [Filter StepTimer])
    Sql.deleteWhere ([] :: [Filter TotalTimer])
    Sql.deleteWhere ([] :: [Filter Run])

resetModels :: MonadSql m => m ()
resetModels = do
    Sql.deleteWhere ([] :: [Filter PredictionModel])
    Sql.deleteWhere ([] :: [Filter ModelGraphProperty])
    Sql.deleteWhere ([] :: [Filter ModelStepProperty])
    Sql.deleteWhere ([] :: [Filter UnknownPrediction])
    Sql.deleteWhere ([] :: [Filter UnknownSet])

commands :: String -> Command (Input SqlM ())
commands name = CommandGroup CommandInfo
  { commandName = name
  , commandHeaderDesc = "a tool for registering and running GPU benchmarks"
  , commandDesc =
        "Register GPUs, algorithms, algorithm implementations, and graphs in \
        \an SQLite database of configurations. Automatically run missing \
        \configurations and store all results in the database."
  } [ addCommands
    , resetCommands
    , SingleCommand CommandInfo
        { commandName = "run-benchmarks"
        , commandHeaderDesc = "run benchmarks"
        , commandDesc = "Run benchmarks for registered configurations"
        }
        $ runBenchmarks <$> parallelism
    , SingleCommand CommandInfo
        { commandName = "import-results"
        , commandHeaderDesc = "import results of external tools"
        , commandDesc = "Import results of external implementations"
        }
        $ pure importResults
    ]
  where
    parallelism = option auto . mconcat $ [ metavar "N", short 'j', value 2 ]

addCommands :: Command (Input SqlM ())
addCommands = CommandGroup CommandInfo
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
    ]
  where
    graphFile = metavar "GRAPH" <> help "Graph file"

resetCommands :: Command (Input SqlM ())
resetCommands = CommandGroup CommandInfo
    { commandName = "reset"
    , commandHeaderDesc = "commands for resetting database"
    , commandDesc = "Reset or drop database entries"
    }
    [ SingleCommand CommandInfo
        { commandName = "retries"
        , commandHeaderDesc = "resets the retry count"
        , commandDesc = "Reset the retry count for failed experiments"
        }
        $ pure resetRetries
    , SingleCommand CommandInfo
        { commandName = "properties"
        , commandHeaderDesc = "deletes logged properties"
        , commandDesc = "Deletes the properties stored for each variant"
        }
        $ pure resetProperties
    , SingleCommand CommandInfo
        { commandName = "measurements"
        , commandHeaderDesc = "deletes timing measurements"
        , commandDesc = "Delete all timing measurements"
        }
        $ pure resetMeasurements
    , SingleCommand CommandInfo
        { commandName = "models"
        , commandHeaderDesc = "deletes models"
        , commandDesc = "Delete all stored models"
        }
        $ pure resetModels
    ]


main :: IO ()
main = runSqlM commands $ runInput
