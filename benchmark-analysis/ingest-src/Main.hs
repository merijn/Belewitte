{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main(main) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Conduit (ConduitT, Void, (.|), await, runConduit)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Monoid ((<>))
import qualified Data.Text as T

import qualified Commands.Add as Add
import qualified Commands.Reset as Reset
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
import Sql (Entity(..), MonadSql, (==.))
import qualified Sql

main :: IO ()
main = runSqlM commands $ runInput

commands :: String -> Command (Input SqlM ())
commands name = CommandGroup CommandInfo
  { commandName = name
  , commandHeaderDesc = "a tool for registering and running GPU benchmarks"
  , commandDesc =
        "Register GPUs, algorithms, algorithm implementations, and graphs in \
        \an SQLite database of configurations. Automatically run missing \
        \configurations and store all results in the database."
  } [ Add.commands
    , lift <$> Reset.commands
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
queryTest outputSuffix = lift . runConduit $ do
    Sql.selectKeys [] [] .> \runConfigId ->
        runSqlQuery (missingQuery runConfigId) .| querySink outputSuffix
  where
    querySink
        :: (MonadResource m, MonadThrow m, Show a)
        => Maybe String -> ConduitT a Void m ()
    querySink Nothing = void await
    querySink (Just suffix) =
        C.map showText
        .| C.map (`T.snoc` '\n')
        .| C.encode C.utf8
        .| C.sinkFile ("missingQuery-" <> suffix)

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
          ExternalTimer platId varId implId algoId name minTime avgTime maxTime stddev

