{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main(main) where

import BroadcastChan.Conduit
import Control.Monad (unless, void)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Conduit (ConduitT, Void, ZipConduit(..), (.|), runConduit)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Foldable (asum)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitFailure)

import qualified Commands.Add as Add
import qualified Commands.Import as Import
import qualified Commands.List as List
import qualified Commands.Query as Query
import qualified Commands.Reset as Reset
import qualified Commands.Set as Set
import qualified Commands.Unset as Unset
import Core
import InteractiveInput
import Jobs
import Options
import Parsers
import JobPool
import Query (Region, streamQuery, runSqlQueryConduit)
import Query.Field (getDistinctFieldQuery)
import Query.Missing
import RuntimeData (OutputDiff(..))
import qualified RuntimeData
import Schema
import Sql (MonadSql, SelectOpt(Asc), Transaction, (==.), (||.))
import qualified Sql
import qualified Sql.Transaction as SqlTrans

main :: IO ()
main = runInputCommand_ CommandRoot
  { mainHeaderDesc = "a tool for registering and running GPU benchmarks"
  , mainDesc =
        "Register GPUs, algorithms, algorithm implementations, and graphs in \
        \an SQLite database of configurations. Automatically run missing \
        \configurations and store all results in the database."
  , mainQueryDump = ingestQueryDump
  , mainQueryMap = ingestQueryMap
  , mainCommands = SubCommands
    [ Add.commands
    , lift <$> Set.commands
    , lift <$> Unset.commands
    , lift <$> Reset.commands
    , lift <$> List.commands
    , lift <$> Query.commands
    , SingleCommand CommandInfo
        { commandName = "run-benchmarks"
        , commandHeaderDesc = "run benchmarks"
        , commandDesc = "Run benchmarks for registered configurations"
        }
        $ pure runBenchmarks
    , SingleCommand CommandInfo
        { commandName = "validate"
        , commandHeaderDesc = "validate measurement results"
        , commandDesc =
            "Validate result mismatches for non-deterministic algorithms"
        }
        $ pure validate
    , lift <$> Import.commands
    , SingleCommand CommandInfo
        { commandName = "import-results"
        , commandHeaderDesc = "import results of external tools"
        , commandDesc = "Import results of external implementations"
        }
        $ pure importResults
    ]
  }

-- Error out if C++ code hasn't been compiled
checkCxxCompiled :: SqlM ()
checkCxxCompiled = void $ do
    RuntimeData.getKernelExecutable
    RuntimeData.getKernelLibPath

pipelineHandler
    :: MonadLogger m => (a -> Key Platform) -> Handler m a
pipelineHandler f = Handle (handler . f)
  where
    handler :: MonadLogger m => Key Platform -> SomeException -> m Action
    handler val exc = Drop <$ do
        logErrorN $ mconcat
            [ "Error while processing platform: "
            , showSqlKey val, "\n"
            , T.pack $ displayException exc
            ]

runBenchmarks :: Input SqlM ()
runBenchmarks = lift $ do
    checkCxxCompiled

    Entity _ defaultPlatform <- tryAlternatives
        [ MaybeT $ Sql.selectFirst [PlatformIsDefault ==. Active] []
        , MaybeT $ do
            logWarnN "No default platform specified!"
            Sql.selectFirst [] []
        ]

    let numDefaultNodes = platformAvailable defaultPlatform
        variantFilters = [VariantPropsStored ==. False] ||.
                         [VariantResult ==. Nothing]

    Sql.selectSource variantFilters [] $
        C.concatMapM variantToPropertyJob
        .| processJobsParallel numDefaultNodes defaultPlatform
        .| C.mapM_ processProperty

    platformQuery <- getDistinctFieldQuery RunConfigPlatformId

    runSqlQueryConduit platformQuery $
        parMapM_ (pipelineHandler id) 10 processPlatformRunConfigs
  where
    processPlatformRunConfigs :: Key Platform -> SqlM ()
    processPlatformRunConfigs platformId = do
        platform <- Sql.getJust platformId

        Sql.selectSource [RunConfigPlatformId ==. platformId] [] $
            C.mapM_ (runRunConfigs platform)
      where
        runRunConfigs :: Platform -> Entity RunConfig -> SqlM ()
        runRunConfigs platform (Entity runCfgId cfg) = do
            unless (runConfigPlatformId cfg == platformId) $ do
                logThrowM . GenericInvariantViolation $ mconcat
                    [ "Query for platform #", showSqlKey platformId
                    , " returned run config for different platform!"
                    ]

            runSqlQueryConduit (missingBenchmarkQuery FilterRetries runCfgId) $
                C.concatMapM (missingRunToTimingJob platformId)
                .| processJobsParallel numNodes platform
                .| C.mapM_ (processTiming runCfgId commitId)
          where
            commitId = runConfigAlgorithmVersion cfg
            numNodes = platformAvailable platform

    tryAlternatives :: (MonadIO m, MonadLogger m) => [MaybeT m a] -> m a
    tryAlternatives alts = runMaybeT (asum alts) >>= maybe exitOnNothing return

    exitOnNothing :: (MonadIO m, MonadLogger m) => m a
    exitOnNothing = do
        logErrorN "No platforms registered!"
        liftIO $ exitFailure

validate :: Input SqlM ()
validate = lift $ do
    checkCxxCompiled
    diff <- Sql.selectSource [] [] $
        parMapM (pipelineHandler entityKey) 10 validateForPlatform .| C.fold

    liftIO . T.putStrLn $ mconcat
        [ "Overall max difference:\n", RuntimeData.renderOutputDiff diff ]
  where
    validateForPlatform :: Entity Platform -> SqlM OutputDiff
    validateForPlatform (Entity platformId platform) =
        withProcessPool numNodes platform $ \procPool -> Sql.runRegionConduit $
            streamQuery (validationVariantQuery platformId)
            .| C.map validationVariantToJob
            .| toRegion (processJobsParallelWithSharedPool numNodes procPool)
            .> validationMissingRuns platformId
            .| toRegion (processJobsParallelWithSharedPool numNodes procPool)
            .| toRegion (validateResults numNodes)
            .| toRegion (C.foldMapM cleanupValidation)
      where
        numNodes = platformAvailable platform

        validationVariantToJob :: ValidationVariant -> Job ValidationVariant
        validationVariantToJob config@ValidationVariant{..} = makeTimingJob
            config
            validationVariantId
            Nothing
            ("-k switch" : validationArgs)

        toRegion :: Monad m => ConduitT a b m r -> ConduitT a b (Region m) r
        toRegion = C.transPipe lift

importResults :: Input SqlM ()
importResults = do
    Entity platformId _ <- getInteractive platformInput "Platform Name"
    Entity algoId _ <- getInteractive algoInput "Algorithm Name"
    Entity implId _ <- getInteractive (implInput algoId) "Implementation Name"
    Entity varCfgId _ <- getInteractive (varCfgInput algoId) "Variant Config Name"

    filepath <- getInteractive filepathInput "Result File"

    lift . SqlTrans.runTransaction . runConduit $
        C.sourceFile filepath
        .| C.decode C.utf8
        .| conduitParse externalResult
        .| C.mapM_ (insertResult platformId algoId implId varCfgId)
  where
    platformInput = sqlInput PlatformName UniqPlatform
    algoInput = sqlInput AlgorithmName UniqAlgorithm
    implInput algoId = sqlInput ExternalImplName (UniqExternalImpl algoId)
    varCfgInput algoId = sqlInput VariantConfigName (UniqVariantConfigName algoId)

    insertResult
        :: (MonadLogger m, MonadSql m, MonadThrow m)
        => Key Platform
        -> Key Algorithm
        -> Key ExternalImpl
        -> Key VariantConfig
        -> ExternalResult
        -> Transaction m ()
    insertResult platId algoId implId varCfgId (ExternalResult gname dataset Timing{..}) = do
        datasetId <- SqlTrans.validateKey dataset
        graphId <- SqlTrans.getBy (UniqGraphName gname datasetId) >>= \case
            Just (Entity key _) -> return key
            Nothing -> logThrowM . PatternFailed $
                "Graph not found for specified dataset"

        varId <- SqlTrans.getBy (UniqVariant graphId varCfgId) >>= \case
            Just (Entity key _) -> return key
            Nothing -> logThrowM . PatternFailed $
                "Variant not found for specified graph"

        SqlTrans.insert_ $
          ExternalTimer platId varId implId algoId name minTime avgTime maxTime stddev

ingestQueryDump :: FilePath -> SqlM ()
ingestQueryDump outputSuffix = do
    Sql.runRegionConduit $
        Sql.selectKeysRegion [] [Asc RunConfigId]
        .| C.map (missingBenchmarkQuery FilterRetries)
        .> streamQuery
        .| querySink "missingQuery-"

    let querySink1 = ZipConduit $ querySink "validationVariantQuery-"
        querySink2 = ZipConduit $
            C.map validationRunQuery
            .> C.fuse (Sql.selectKeysRegion [] [Asc PlatformId]) . C.map
            .> streamQuery
            .| querySink "validationRunQuery-"

        finalSink = getZipConduit $ mappend <$> querySink1 <*> querySink2

    Sql.runRegionConduit $
        Sql.selectKeysRegion [] [Asc PlatformId]
        .| C.map validationVariantQuery
        .> streamQuery
        .| finalSink
  where
    querySink
        :: (MonadResource m, MonadThrow m, Show a)
        => String -> ConduitT a Void m ()
    querySink name =
        C.map showText
        .| C.map (`T.snoc` '\n')
        .| C.encode C.utf8
        .| C.sinkFile (name <> outputSuffix)

ingestQueryMap :: Map String (Parser DebugQuery)
ingestQueryMap = M.fromList
    [ nameDebugQuery "missingBenchmarkQuery" $
        missingBenchmarkQuery FilterRetries <$> Compose runconfigIdParser
    , nameDebugQuery "validationVariantQuery" $
        validationVariantQuery <$> Compose platformIdParser
    , nameDebugQuery "validationRunQuery" $
        validationRunQuery <$> Compose validationVariantParser
                           <*> Compose platformIdParser
    ]
  where
    validationVariantParser = do
        getAlgoId <- algorithmIdParser
        getVariantId <- variantIdParser
        getCommitId <- commitIdParser
        pure $ do
            algoId <- getAlgoId
            ValidationVariant algoId
                <$> getVariantId algoId <*> getCommitId algoId <*> pure 0
                <*> pure []
