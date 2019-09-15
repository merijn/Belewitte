{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main(main) where

import Control.Monad (mzero, unless, void)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Char (isSpace)
import Data.Conduit (ConduitT, Void, (.|), await, runConduit)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Monoid ((<>))
import qualified Data.Text as T
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
    insertVariants graphId = Sql.selectKeys [] [] .| C.mapM_ insertVariant
      where
        insertVariant :: MonadSql m => Key VariantConfig -> m ()
        insertVariant variantConfigId =
            Sql.insert_ $ Variant graphId variantConfigId Nothing False 0

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
    isDefault <- getInteractive readInput "Default Variant (for plotting)"
    varCfgId <- Sql.insert $ VariantConfig algoId variantName flags isDefault

    let mkVariant gId = Variant gId varCfgId Nothing False 0

    runConduit $ Sql.selectKeys [] [] .| C.mapM_ (Sql.insert_ . mkVariant)
  where
    algoInput = sqlInput AlgorithmName UniqAlgorithm

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

importResults :: Input SqlM ()
importResults = do
    Entity platformId _ <- getInteractive platformInput "Platform Name"
    Entity algoId _ <- getInteractive algoInput "Algorithm Name"
    Entity implId _ <- getInteractive (implInput algoId) "Implementation Name"

    filepath <- getInteractive filepathInput "Result File"

    runConduit $
        C.sourceFile filepath
        .| C.decode C.utf8
        .| C.map (T.replace "," "")
        .| conduitParse externalResult
        .| C.mapM_ (insertResult platformId algoId implId)
  where
    platformInput = sqlInput PlatformName UniqPlatform
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    implInput algoId = sqlInput ExternalImplName (UniqExternalImpl algoId)

    insertResult
        :: (MonadSql m, MonadTagFail m)
        => Key Platform
        -> Key Algorithm
        -> Key ExternalImpl
        -> ExternalResult
        -> m ()
    insertResult platId algoId implId (ExternalResult gname varName Timing{..}) = do
        [graphId] <- logIfFail "More than one graph found for" gname $
            Sql.selectKeysList [GraphName ==. gname] []

        let uniqVariantConfig = UniqVariantConfig algoId varName

        Just varCfgId <- logIfFail "No variant config found" varName $
            fmap entityKey <$> Sql.getBy uniqVariantConfig

        let uniqVariant = UniqVariant graphId varCfgId

        Just varId <- logIfFail "No variant found" varCfgId $
            fmap entityKey <$> Sql.getBy uniqVariant

        Sql.insert_ $
          ExternalTimer platId varId implId name minTime avgTime maxTime stddev

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

queryTest :: Maybe FilePath -> Input SqlM ()
queryTest outputSuffix = lift $ do
    runConduit $
        Sql.selectKeys [] []
        .> \runConfigId ->
        runSqlQuery (missingQuery runConfigId)
        .| querySink outputSuffix
  where
    querySink
        :: (MonadResource m, MonadThrow m, Show a)
        => Maybe String -> ConduitT a Void m ()
    querySink Nothing = void await
    querySink (Just suffix) =
        C.map showText
        .| C.map (`T.snoc` '\n')
        .| C.encode C.utf8
        .| C.sinkFile ("missingQuery" <> suffix)

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
    , HiddenCommand CommandInfo
        { commandName = "query-test"
        , commandHeaderDesc = "check query output"
        , commandDesc = "Dump query output to files to validate results"
        }
        $ queryTest <$> (suffixParser <|> pure Nothing)
    ]
  where
    parallelism = option auto . mconcat $ [ metavar "N", short 'j', value 2 ]

    suffixReader :: String -> Maybe (Maybe String)
    suffixReader "" = Nothing
    suffixReader s
        | any isSpace s = Nothing
        | otherwise = Just $ Just s

    suffixParser :: Parser (Maybe String)
    suffixParser = argument (maybeReader suffixReader) . mconcat $
        [ metavar "SUFFIX" ]

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
    , SingleCommand CommandInfo
        { commandName = "run-config"
        , commandHeaderDesc = "register a new run configuration"
        , commandDesc = "Register a new configuration of benchmarks to run."
        }
        $ pure addRunConfig
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
