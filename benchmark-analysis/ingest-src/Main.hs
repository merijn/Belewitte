{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad ((>=>), forM_, guard, when)
import Control.Monad.Logger (logErrorN, logInfoN)
import Control.Monad.Trans (lift)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash.Conduit (hashFile)
import qualified Data.ByteArray (convert)
import Data.Conduit (ConduitT, (.|), awaitForever, runConduit, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sqlite (Key, Entity(..), (=.), (==.))
import qualified Database.Persist.Sqlite as Sql
import GHC.Conc.Sync (getNumProcessors, setNumCapabilities)
import Options.Applicative
import System.Directory (removeFile, doesFileExist)
import System.Environment (getProgName)
import System.FilePath (splitExtension, takeFileName)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import BroadcastChan.Conduit
import Parsers
import ProcessPool
import Schema

computeHash :: MonadIO m => FilePath -> m Hash
computeHash path = do
    (digest :: Digest MD5) <- hashFile path
    return . Hash . Data.ByteArray.convert $ digest

runTask :: Process -> (a, b, c, Text) -> IO (a, b, c, Text)
runTask Process{..} (x, y, z, cmd) = do
    -- FIXME proper logging
    T.putStrLn $ "Task: " <> cmd
    T.hPutStrLn inHandle cmd
    result <- T.hGetLine outHandle
    T.putStrLn $ "Done: " <> cmd
    return (x, y, z, result)

propertyJobs :: ConduitT () (Key Variant, Key Graph, Maybe Hash, Text) SqlM ()
propertyJobs = Sql.selectSource [] [] .| awaitForever toPropJob
  where
    toPropJob
        :: Entity Variant
        -> ConduitT (Entity Variant)
                    (Key Variant, Key Graph, Maybe Hash, Text)
                    SqlM
                    ()
    toPropJob (Entity varId (Variant graphId algoId _ varFlags hash)) = do
        whenNotExists filters $ do
            Graph _ path _ <- lift $ Sql.getJust graphId
            Algorithm algo _ <- lift $ Sql.getJust algoId
            yield . (varId, graphId, hash,) . T.intercalate " " $
                [ showSqlKey varId
                , "-a", algo
                , "-k switch --log"
                , showSqlKey varId <> ".log"
                , fromMaybe "" varFlags
                , path
                ]
      where
        filters = [StepPropVariantId ==. varId, StepPropStepId ==. 0]

processProperty :: (Key Variant, Key Graph, Maybe Hash, Text) -> SqlM ()
processProperty (varId, graphId, hash, var) = do
    logInfoN $ "Property: " <> var
    liftIO $ removeFile timingFile

    when (isNothing hash) $ do
        resultHash <- computeHash outputFile
        Sql.update varId [VariantResult =. Just resultHash]
        liftIO $ removeFile outputFile

    runConduit $
        C.sourceFile propLog
        .| C.decode C.utf8
        .| C.map (T.replace "," "")
        .| conduitParse property
        .| C.mapM_ insertProperty

    liftIO $ removeFile propLog
    logInfoN $ "Property done: " <> var
  where
    propLog :: FilePath
    propLog = T.unpack var <> ".log"

    timingFile :: FilePath
    timingFile = T.unpack var <> ".timings"

    outputFile :: FilePath
    outputFile = T.unpack var <> ".output"

    insertProperty :: Property -> SqlM ()
    insertProperty (GraphProperty name val) =
        insertUniq $ GraphProp graphId name val

    insertProperty (StepProperty n name val) =
        Sql.insert_ $ StepProp varId n name val

    insertProperty Prediction{} = return ()

timingJobs
    :: Int -> Key GPU
    -> ConduitT () (Key Variant, Key Implementation, Hash, Text) SqlM ()
timingJobs numRuns gpuId = do
    impls <- lift $ Sql.selectList [ImplementationRunnable ==. True] []
    Sql.selectSource [] [] .| awaitForever (toTimingJob impls)
  where
    toTimingJob
        :: [Entity Implementation]
        -> Entity Variant
        -> ConduitT (Entity Variant)
                    (Key Variant, Key Implementation, Hash, Text)
                    SqlM
                    ()
    toTimingJob _ (Entity varId (Variant graphId algoId _ _ Nothing)) = do
        logErrorN . mconcat $
            [ "Algorithm #", showText algoId, " results missing for graph #"
            , showText graphId, " variant #", showText varId ]

    toTimingJob impls (Entity varId (Variant graphId algoId _ varFlags (Just hash))) =
        forM_ impls runImpl
      where
        runImpl (Entity implId (Implementation _ name _ implFlags _ _)) = do
            whenNotExists (filters ++ [TotalTimerImplId ==. implId]) $ do
                Graph _ path _ <- lift $ Sql.getJust graphId
                Algorithm algo _ <- lift $ Sql.getJust algoId

                yield . (varId, implId, hash,) . T.intercalate " " $
                    [ tag name , "-a"
                    , algo
                    , fromMaybe ("-k " <> name) implFlags
                    , fromMaybe "" varFlags
                    , "-n " <> showText numRuns
                    , path
                    ]

        filters = [TotalTimerGpuId ==. gpuId, TotalTimerVariantId ==. varId]
        tag name = mconcat ["\"", showSqlKey varId, " ", name, "\""]

processTiming
    :: Key GPU -> (Key Variant, Key Implementation, Hash, Text) -> SqlM ()
processTiming gpuId (varId, implId, hash, var) = do
    logInfoN $ "Timing: " <> var
    resultHash <- computeHash outputFile

    if resultHash /= hash
       then do
           logErrorN . mconcat $
            [ "Implementation #", showText implId
            , " has wrong results for variant #", showText varId, " on GPU #"
            , showText gpuId ]
        else do
            liftIO $ removeFile outputFile
            runConduit $
                C.sourceFile timingFile
                .| C.decode C.utf8
                .| C.map (T.replace "," "")
                .| conduitParse timer
                .| C.mapM_ insertTiming
            liftIO $ removeFile timingFile

    logInfoN $ "Timing done: " <> var
  where
    timingFile = T.unpack var <> ".timings"
    outputFile = T.unpack var <> ".output"

    insertTiming :: Timer -> SqlM ()
    insertTiming (TotalTiming Timing{..}) = Sql.insert_ $
        TotalTimer gpuId varId implId name minTime avgTime maxTime stddev

    insertTiming (StepTiming n Timing{..}) = Sql.insert_ $
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
    Sql.insert_ $ GPU gpuName prettyName

addGraphs :: [FilePath] -> SqlM ()
addGraphs = mapM_ $ \path -> do
    liftIO $ doesFileExist path >>= guard
    let (graphName, ext) = splitExtension $ takeFileName path
    liftIO $ guard (ext == ".graph")
    Sql.insert_ $ Graph (T.pack graphName) (T.pack path) Nothing

addAlgorithm :: SqlM ()
addAlgorithm = do
    Just algoName <- queryLine "Algorithm Name"
    prettyName <- queryLine "Algorithm Pretty Name"
    Sql.insert_ $ Algorithm algoName prettyName

addImplementation :: SqlM ()
addImplementation = do
    Just algoName <- queryLine "Algorithm Name"
    Just (Entity algoId _) <- Sql.getBy (UniqAlgorithm algoName)
    Just implName <- queryLine "Implementation Name"
    prettyName <- queryLine "Implementation Pretty Name"
    flags <- queryLine "Flags"
    Just algoType <- queryRead "Algorithm type"
    Just runnable <- queryRead "Runnable"
    Sql.insert_ $
        Implementation algoId implName prettyName flags algoType runnable

addVariant :: SqlM ()
addVariant = do
    Just algoName <- queryLine "Algorithm Name"
    Just (Entity algoId _) <- Sql.getBy (UniqAlgorithm algoName)
    Just variantName <- queryLine "Variant Name"
    flags <- queryLine "Flags"
    let mkVariant gId = Variant gId algoId variantName flags Nothing
    runConduit $
        Sql.selectKeys [] []
        .| C.mapM_ (Sql.insert_ . mkVariant)

importResults :: SqlM ()
importResults = do
    Just gpuName <- queryLine "GPU Name"
    Just (Entity gpuId _) <- Sql.getBy (UniqGPU gpuName)
    Just algoName <- queryLine "Algorithm Name"
    Just (Entity algoId _) <- Sql.getBy (UniqAlgorithm algoName)
    Just implName <- queryLine "Implementation Name"
    Just (Entity implId _) <- Sql.getBy (UniqImpl algoId implName)
    Just filepath <- queryLine "Result file"
    runConduit $
        C.sourceFile (T.unpack filepath)
        .| C.decode C.utf8
        .| C.map (T.replace "," "")
        .| conduitParse externalResult
        .| C.mapM_ (insertResult gpuId algoId implId)
  where
    insertResult
        :: Key GPU
        -> Key Algorithm
        -> Key Implementation
        -> ExternalResult
        -> SqlM ()
    insertResult gpuId algoId implId (Result gname variantName Timing{..}) = do
        [graphId] <- logIfFail "More than one graph found for" gname $
            Sql.selectKeysList [GraphName ==. gname] []

        let uniqVariant = UniqVariant graphId algoId varName

        Just varId <- logIfFail "No variant found" varName $
            fmap entityKey <$> Sql.getBy uniqVariant

        Sql.insert_ $
            TotalTimer gpuId varId implId name minTime avgTime maxTime stddev
      where
        --FIXME get from command
        varName
            | variantName == "0" = "default"
            | otherwise = "Root " <> variantName


runBenchmarks :: Int -> Int -> SqlM ()
runBenchmarks numNodes numRuns = do
    -- FIXME hardcoded GPU
    withProcessPool numNodes (GPU "TitanX" Nothing) $ \procPool -> runConduit $
        propertyJobs
        .| parMapM (Simple Retry) numNodes (withProcess procPool runTask)
        .| C.mapM_ processProperty

    gpus <- Sql.selectList [] []
    forM_ gpus $ \(Entity gpuId gpu) -> do
        withProcessPool numNodes gpu $ \procPool -> runConduit $
            timingJobs numRuns gpuId
            .| parMapM (Simple Retry) numNodes (withProcess procPool runTask)
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
    , subCommand "import-results" "import results of external tools" "" $
        pure importResults
    , subCommand "run-benchmarks" "run benchmarks"
                 "Run benchmarks for missing registered configurations" $
                 runBenchmarks <$> parallelism <*> numRuns
    ]
  where
    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ name ++ " " ++ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

    graphFile = metavar "GRAPH" <> help "Graph file"
    parallelism = option auto . mconcat $ [ metavar "N", short 'j', value 2 ]
    numRuns = option auto . mconcat $ [ metavar "N", short 'n', value 30 ]


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
