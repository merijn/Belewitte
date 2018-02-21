{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad ((>=>), forM_, guard)
import Control.Monad.Trans (lift)
import Data.Conduit (ConduitT, (.|), awaitForever, runConduit, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sqlite (Key, Entity(..), (==.), getJust, insert_)
import qualified Database.Persist.Sqlite as Sql
import GHC.Conc.Sync (getNumProcessors, setNumCapabilities)
import Options.Applicative
import System.Directory (removeFile, doesFileExist)
import System.Environment (getProgName)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (splitExtension, takeFileName)
import System.IO (hFlush, stdout)
import System.Process (createProcess, proc, waitForProcess)
import Text.Read (readMaybe)

import BroadcastChan.Conduit
import Parsers
import ProcessPool
import Schema

runTask :: Process -> (a, b, Text) -> IO (a, b, Text)
runTask Process{..} (x, y, cmd) = do
    T.putStrLn $ "Task: " <> cmd
    T.hPutStrLn inHandle cmd
    result <- T.hGetLine outHandle
    T.putStrLn $ "Done: " <> cmd
    return (x, y, result)

propertyJobs :: ConduitT () (Key Variant, Key Graph, Text) SqlM ()
propertyJobs = Sql.selectSource [] [] .| awaitForever toPropJob
  where
    toPropJob
        :: Entity Variant
        -> ConduitT (Entity Variant) (Key Variant, Key Graph, Text) SqlM ()
    toPropJob (Entity varId (Variant graphId algoId _ varFlags)) = do
        whenNotExists filters $ do
            Graph _ path _ <- lift $ getJust graphId
            Algorithm algo _ <- lift $ getJust algoId
            yield . (varId, graphId,) . T.intercalate " " $
                [ showSqlKey varId
                , "-a", algo
                , "-k switch --log"
                , showSqlKey varId <> ".log"
                , fromMaybe "" varFlags
                , path
                ]
      where
        filters = [StepPropVariantId ==. varId, StepPropStepId ==. 0]

processProperty :: (Key Variant, Key Graph, Text) -> SqlM ()
processProperty (varId, graphId, var) = do
    liftIO $ do
        putStrLn $ "Property: " ++ T.unpack var
        removeFile timingFile

    runConduit $
        C.sourceFile propLog
        .| C.decode C.utf8
        .| C.map (T.replace "," "")
        .| conduitParse property
        .| C.mapM_ insertProperty

    liftIO $ do
        removeFile propLog
        putStrLn $ "Property done: " ++ T.unpack var
  where
    propLog :: FilePath
    propLog = T.unpack var <> ".log"

    timingFile :: FilePath
    timingFile = T.unpack var <> ".timings"

    insertProperty :: Property -> SqlM ()
    insertProperty (GraphProperty name val) =
        insertUniq $ GraphProp graphId name val

    insertProperty (StepProperty n name val) =
        insert_ $ StepProp varId n name val

    insertProperty Prediction{} = return ()

timingJobs
    :: Key GPU -> ConduitT () (Key Variant, Key Implementation, Text) SqlM ()
timingJobs gpuId = do
    impls <- lift $ Sql.selectList [ImplementationRunnable ==. True] []
    Sql.selectSource [] [] .| awaitForever (toTimingJob impls)
  where
    toTimingJob
        :: [Entity Implementation]
        -> Entity Variant
        -> ConduitT (Entity Variant)
                   (Key Variant, Key Implementation, Text)
                   SqlM
                   ()
    toTimingJob impls (Entity varId (Variant graphId algoId _ varFlags)) =
        forM_ impls runImpl
      where
        runImpl (Entity implId (Implementation _ name _ implFlags _ _)) = do
            whenNotExists (filters ++ [TotalTimerImplId ==. implId]) $ do
                Graph _ path _ <- lift $ getJust graphId
                Algorithm algo _ <- lift $ getJust algoId

                yield . (varId, implId,) . T.intercalate " " $
                    [ tag name , "-a"
                    , algo
                    , fromMaybe ("-k " <> name) implFlags
                    , fromMaybe "" varFlags
                    , "-n 30"
                    , path
                    ]

        filters = [TotalTimerGpuId ==. gpuId, TotalTimerVariantId ==. varId]
        tag name = mconcat ["\"", showSqlKey varId, " ", name, "\""]

processTiming :: Key GPU -> (Key Variant, Key Implementation, Text) -> SqlM ()
processTiming gpuId (varId, implId, var) = do
    result <- liftIO $ do
        putStrLn $ "Timing: " ++ T.unpack var
        (Nothing, Nothing, Nothing, hnd) <- createProcess compareProc
        waitForProcess hnd

    case result of
        ExitFailure _ -> liftIO $ putStrLn "whoops!"
        ExitSuccess -> do
            liftIO $ removeFile outputFile
            runConduit $
                C.sourceFile timingFile
                .| C.decode C.utf8
                .| C.map (T.replace "," "")
                .| conduitParse timing
                .| C.mapM_ insertTiming
            liftIO $ removeFile timingFile

    liftIO . putStrLn $ "Timing done: " ++ T.unpack var
  where
    timingFile = T.unpack var <> ".timings"
    outputFile = T.unpack var <> ".output"
    reference = T.unpack $ T.takeWhile (/=' ') var <> ".output"
    compareProc = proc "diff" ["-q", reference, outputFile]

    insertTiming :: Timer -> SqlM ()
    insertTiming (TotalTiming Timing{..}) = insert_ $
        TotalTimer gpuId varId implId name minTime avgTime maxTime stddev

    insertTiming (StepTiming n Timing{..}) = insert_ $
        StepTimer gpuId varId n implId name minTime avgTime maxTime stddev

queryLineWith :: MonadIO m => (Text -> Maybe a) -> Text -> m (Maybe a)
queryLineWith convert prompt = liftIO $ do
    T.putStr $ prompt <> ": "
    hFlush stdout
    (checkEmpty >=> convert) <$> T.getLine
  where
    checkEmpty :: Text -> Maybe Text
    checkEmpty t = if T.null t then Nothing else Just t

queryLine :: MonadIO m => Text -> m (Maybe Text)
queryLine = queryLineWith return

queryRead :: (MonadIO m, Read a) => Text -> m (Maybe a)
queryRead = queryLineWith (readMaybe . T.unpack)

addGPU :: SqlM ()
addGPU = do
    Just gpuName <- queryLine "GPU Slurm Name"
    prettyName <- queryLine "GPU Pretty Name"
    insert_ $ GPU gpuName prettyName

addGraphs :: [FilePath] -> SqlM ()
addGraphs = mapM_ $ \path -> do
    liftIO $ doesFileExist path >>= guard
    let (graphName, ext) = splitExtension $ takeFileName path
    liftIO $ guard (ext == ".graph")
    insert_ $ Graph (T.pack graphName) (T.pack path) Nothing

addAlgorithm :: SqlM ()
addAlgorithm = do
    Just algoName <- queryLine "Algorithm Name"
    prettyName <- queryLine "Algorithm Pretty Name"
    insert_ $ Algorithm algoName prettyName

addImplementation :: SqlM ()
addImplementation = do
    Just algoName <- queryLine "Algorithm Name"
    Just (Entity algoId _) <- Sql.getBy (UniqAlgorithm algoName)
    Just implName <- queryLine "Implementation Name"
    prettyName <- queryLine "Implementation Pretty Name"
    flags <- queryLine "Flags"
    Just algoType <- queryRead "Algorithm type"
    Just runnable <- queryRead "Runnable"
    insert_ $ Implementation algoId implName prettyName flags algoType runnable

addVariant :: SqlM ()
addVariant = do
    Just algoName <- queryLine "Algorithm Name"
    Just (Entity algoId _) <- Sql.getBy (UniqAlgorithm algoName)
    Just variantName <- queryLine "Variant Name"
    flags <- queryLine "Flags"
    runConduit $
        Sql.selectKeys [] []
        .| C.mapM_ (\gId -> insert_ $ Variant gId algoId variantName flags)

runBenchmarks :: Int -> SqlM ()
runBenchmarks n = do
    withProcessPool n (GPU "TitanX" Nothing) $ \procPool -> runConduit $
        propertyJobs
        .| parMapM (Simple Retry) n (withProcess procPool runTask)
        .| C.mapM_ processProperty

    gpus <- Sql.selectList [] []
    forM_ gpus $ \(Entity gpuId gpu) -> do
        withProcessPool n gpu $ \procPool -> runConduit $
            timingJobs gpuId
            .| parMapM (Simple Retry) n (withProcess procPool runTask)
            .| C.mapM_ (processTiming gpuId)

data Options = Options
    { dbPath :: Text
    , verbosity :: LogLevel
    , ingestTask :: SqlM ()
    }

commandParser :: String -> ParserInfo Options
commandParser name = info (options <**> helper) $ mconcat
    [ fullDesc
    , header $ name ++ " - a tool for registering and running GPU benchmarks"
    , progDesc "Register GPUs, algorithms, algorithm implementations, and \
               \graphs in an SQLite database of configurations. Automatically \
               \run missing configurations and store all results in the \
               \database."
    ]
  where
    options :: Parser Options
    options = Options <$> dbFlag <*> verbosityLevel <*> commands name

    dbFlag = strOption . mconcat $
        [ metavar "DATABASE", short 'd', long "database"
        , value "benchmarks.db", help "Path of SQLite database to use."
        , showDefaultWith T.unpack
        ]

    verbosityLevel = (levels !!) <$> verb
      where
        verb = length <$> vFlag <|> vOption
        levels = LevelError : LevelWarn : LevelInfo : repeat LevelDebug

        vFlag = many . flag' () . mconcat $
            [ short 'v', long "verbose", hidden ]

        vOption = option auto . mconcat $
            [ short 'v', long "verbose", help "Enable more verbose logging."
            , value 0, metavar "N", showDefault ]

commands :: String -> Parser (SqlM ())
commands name = hsubparser $ mconcat
    [ subCommand "add-gpu" "register a new GPU" "" $
        pure addGPU
    , subCommand "add-graphs" "register graphs" "" $
        addGraphs <$> some (strArgument graphFile)
    , subCommand "add-algorithm" "register a new algorithm" "" $
        pure addAlgorithm
    , subCommand "add-implementation"
                 "register a new implementation of an algorithm" "" $
        pure addImplementation
    , subCommand "add-variant" "register a new variant of a graph" "" $
        pure addVariant
    , subCommand "run-benchmarks" "run benchmarks"
                 "Run benchmarks for missing registered configurations" $
                 runBenchmarks <$> parallelism
    ]
  where
    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ name ++ " " ++ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

    graphFile = metavar "GRAPH" <> help "Graph file"

    parallelism = option auto . mconcat $
        [ metavar "N", short 'n', value 2 ]

main :: IO ()
main = do
    getNumProcessors >>= setNumCapabilities
    programName <- getProgName
    Options{..} <- execParser $ commandParser programName

    let logVerbosity :: LogSource -> LogLevel -> Bool
        logVerbosity _ lvl = lvl >= verbosity

    runSqlM logVerbosity dbPath $ do
        Sql.liftPersist $ Sql.runMigrationSilent migrateAll
        ingestTask
