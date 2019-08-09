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
import OptionParsers
import Parsers
import ProcessPool
import qualified RuntimeData
import Schema
import Sql (Entity(..), Filter, Key, MonadSql, (=.), (==.))
import qualified Sql

addPlatform :: Input SqlM ()
addPlatform = do
    platformName <- getInteractive textInput "Platform Slurm Name"
    prettyName <- getInteractive optionalInput "Platform Pretty Name"
    Sql.insert_ $ Platform platformName prettyName

addGraphs :: [FilePath] -> Input SqlM ()
addGraphs paths = do
    datasetTag <- getInteractive textInput "Dataset Tag"
    forM_ paths $ \path -> do
        liftIO $ doesFileExist path >>= guard
        let (graphName, ext) = splitExtension $ takeFileName path
        liftIO $ guard (ext == ".graph")
        Sql.insert_ $ Graph (T.pack graphName) datasetTag (T.pack path) Nothing

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
    runnable <- getInteractive (readInput (const True)) "Runnable"

    Sql.insert_ $
        Implementation algoId implName prettyName flags implType runnable
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
    implInput algoId = sqlInput ImplementationName (UniqImpl algoId)

    insertResult
        :: (MonadSql m, MonadTagFail m)
        => Key Platform
        -> Key Algorithm
        -> Key Implementation
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
          TotalTimer platId varId implId name minTime avgTime maxTime stddev ts
                     Nothing
      where
        --FIXME get from command
        variantName
            | varName == "0" = "default"
            | otherwise = "Root " <> varName

runBenchmarks :: Int -> Int -> Input SqlM ()
runBenchmarks numNodes numRuns = lift $ do
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
        Sql.selectSource [] [] .> \(Entity platformId platform) ->
            Sql.selectKeys [] [] .> \algoKey ->
                Sql.selectSource [VariantAlgorithmId ==. algoKey] []
                .> variantToTimingJob numRuns
                .> filterExistingTimings platformId
                .| processJobsParallel numNodes platform
                .| C.mapM_ (processTiming platformId)

resetRetries :: MonadSql m => m ()
resetRetries = Sql.updateWhere [] [VariantRetryCount =. 0]

resetProperties :: MonadSql m => m ()
resetProperties = do
    Sql.deleteWhere ([] :: [Filter GraphProp])
    Sql.deleteWhere ([] :: [Filter StepProp])

resetMeasurements :: MonadSql m => m ()
resetMeasurements = do
    Sql.deleteWhere ([] :: [Filter StepTimer])
    Sql.deleteWhere ([] :: [Filter TotalTimer])

resetModels :: MonadSql m => m ()
resetModels = do
    Sql.deleteWhere ([] :: [Filter PredictionModel])
    Sql.deleteWhere ([] :: [Filter ModelGraphProperty])
    Sql.deleteWhere ([] :: [Filter ModelStepProperty])
    Sql.deleteWhere ([] :: [Filter UnknownPrediction])
    Sql.deleteWhere ([] :: [Filter UnknownSet])

commands :: String -> (InfoMod a, Parser (Input SqlM ()))
commands name = (,) docs . hsubparser $ mconcat
    [ subCommand "add-platform" "register a new platform"
        "Register a new platform in the database." $ pure addPlatform
    , subCommand "add-graphs" "register graphs"
        "Register new graphs in the database." $
        addGraphs <$> some (strArgument graphFile)
    , subCommand "add-algorithm" "register a new algorithm"
        "Register a new algorithm in the database." $ pure addAlgorithm
    , subCommand "add-implementation" "register a new implementation"
        "Register a new implementation of an algorithm." $
        pure addImplementation
    , subCommand "add-variant" "register a new variant"
        "Register a new variant of an algorithm for a graph." $ pure addVariant
    , subCommand "import-results" "import results of external tools"
        "Import results of external implementations." $ pure importResults
    , subCommand "run-benchmarks" "run benchmarks"
                 "Run benchmarks for missing registered configurations" $
                 runBenchmarks <$> parallelism <*> numRuns
    , subCommand "reset-retries" "resets the retry count"
                 "Reset the retry count for failed experiments" $
                 pure $ resetRetries
    , subCommand "reset-properties" "deletes logged properties"
                 "Deletes the properties stored for each variant" $
                 pure $ resetProperties
    , subCommand "reset-measurements" "deletes timing measurements"
                 "Delete all timing measurements" $
                 pure $ resetMeasurements
    , subCommand "reset-models" "deletes models"
                 "Delete all stored models" $
                 pure $ resetModels
    ]
  where
    docs = mconcat
      [ fullDesc
      , header $ name ++ " - a tool for registering and running GPU benchmarks"
      , progDesc
        "Register GPUs, algorithms, algorithm implementations, and graphs in \
        \an SQLite database of configurations. Automatically run missing \
        \configurations and store all results in the database."
      ]

    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ name ++ " " ++ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

    graphFile = metavar "GRAPH" <> help "Graph file"
    parallelism = option auto . mconcat $ [ metavar "N", short 'j', value 2 ]
    numRuns = option auto . mconcat $ [ metavar "N", short 'n', value 30 ]

main :: IO ()
main = runSqlM commands $ runInput
