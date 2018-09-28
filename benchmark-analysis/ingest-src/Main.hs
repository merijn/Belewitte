{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main(main) where

import Control.Monad (forM_, guard)
import Control.Monad.Catch
    (MonadMask, displayException, fromException, onError, try)
import Control.Monad.Reader (ask, local)
import Control.Monad.Trans (lift)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Function (on)
import Data.Monoid ((<>))
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Sqlite
    (Filter, Key, Entity(..), EntityField, Unique, (=.), (==.), (+=.))
import qualified Database.Persist.Sqlite as Sql
import Lens.Micro.Extras (view)
import System.Console.Haskeline hiding (Handler)
import System.Console.Haskeline.MonadException (throwIO)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((<.>), splitExtension, takeFileName)
import System.IO (hClose)
import System.Process (CreateProcess(std_out), StdStream(CreatePipe))
import qualified System.Process as Process
import Text.Read (readMaybe)

import BroadcastChan.Conduit
import Core
import Jobs
import OptionParsers
import Parsers
import Paths_benchmark_analysis (getDataFileName)
import ProcessPool
import Schema

data Completer m
    = SimpleCompletion (String -> m [Completion])
    | FileCompletion

type Input m = InputT (ReaderT (Completer m) m)

liftSql :: SqlM a -> Input SqlM a
liftSql = lift . lift

withCompletion :: Monad m => Completer m -> Input m a -> Input m a
withCompletion completion = mapInputT $ local (const completion)

withProcessCompletion :: MonadIO m => [String] -> Input m a -> Input m a
withProcessCompletion args act = do
    exePath <- liftIO $ getDataFileName "runtime-data/main"
    kernelDir <- liftIO $ getDataFileName "runtime-data/kernels"
    let process = (Process.proc exePath $ ["-L",kernelDir] ++ args)
            { std_out = CreatePipe }
    withCompletion (SimpleCompletion $ processCompleter process) act
  where
    readOutput Nothing (Just hnd) Nothing procHandle =
      T.hGetContents hnd <* hClose hnd <* Process.waitForProcess procHandle
    readOutput _ _ _ _ = throwM Abort

    processCompleter process s = liftIO $ do
        txt <- Process.withCreateProcess process readOutput
        let relevant = filter (T.isPrefixOf (T.pack s)) $ T.lines txt
        return $ map (simpleCompletion . T.unpack) relevant

getInputWith
    :: MonadException m => (Text -> m (Maybe a)) -> Text -> Text -> InputT m a
getInputWith convert errText prompt = go
  where
    go = getInputLine (T.unpack prompt ++ ": ") >>= \case
            Nothing -> throwIO Abort
            Just s -> lift (convert . T.stripEnd . T.pack $ s) >>= \case
                Just r -> return r
                Nothing -> outputStrLn (T.unpack errText) >> go

getInputWithSqlCompletion
    :: SqlRecord record
    => EntityField record Text
    -> (Text -> Unique record)
    -> Text
    -> Input SqlM (Entity record)
getInputWithSqlCompletion field uniq prompt =
  withCompletion (SimpleCompletion fromQuery) $
    getInputWith (lift . Sql.getBy . uniq) err prompt
  where
    err = "Name not found in database!"
    getFieldValue = view (Sql.fieldLens field)
    toCompletions = map $ simpleCompletion . T.unpack . getFieldValue
    fromQuery s =
      toCompletions <$> Sql.selectList [field `likeFilter` T.pack s] []

getOptionalInput :: MonadException m => Text -> InputT m (Maybe Text)
getOptionalInput = getInputWith checkEmpty "" -- Never fails
  where
    checkEmpty :: MonadException m => Text -> m (Maybe (Maybe Text))
    checkEmpty txt
        | T.null txt = return $ Just Nothing
        | otherwise = return . Just . Just $ txt

getReadInput
    :: forall a m . (MonadException m, Bounded a, Enum a, Read a, Show a)
    => Text -> Input m a
getReadInput prompt = withCompletion (SimpleCompletion completions) $
    getInputWith (return . readMaybe . T.unpack) "Parse error!" prompt
  where
    allValues :: [a]
    allValues = [minBound .. maxBound]

    completions :: Monad m => String -> m [Completion]
    completions s = return $ toCompletions s allValues

    toCompletions :: Show x => String -> [x] -> [Completion]
    toCompletions s = map simpleCompletion . filter (isPrefix s) . map show
      where
        isPrefix = isPrefixOf `on` map toLower

getInput :: MonadException m => Text -> InputT m Text
getInput = getInputWith checkEmpty "Empty input not allowed!"
  where
    checkEmpty :: MonadException m => Text -> m (Maybe Text)
    checkEmpty txt
        | T.null txt = return Nothing
        | otherwise = return $ Just txt

addGPU :: Input SqlM ()
addGPU = do
    gpuName <- getInput "GPU Slurm Name"
    prettyName <- getOptionalInput "GPU Pretty Name"
    liftSql . Sql.insert_ $ GPU gpuName prettyName

addGraphs :: [FilePath] -> SqlM ()
addGraphs = mapM_ $ \path -> do
    liftIO $ doesFileExist path >>= guard
    let (graphName, ext) = splitExtension $ takeFileName path
    liftIO $ guard (ext == ".graph")
    Sql.insert_ $ Graph (T.pack graphName) (T.pack path) Nothing

addAlgorithm :: Input SqlM ()
addAlgorithm = do
    algoName <- withProcessCompletion ["list","algorithms"] $
        getInput "Algorithm Name"
    prettyName <- getOptionalInput "Algorithm Pretty Name"
    liftSql . Sql.insert_ $ Algorithm algoName prettyName

addImplementation :: Input SqlM ()
addImplementation = do
    Entity algoId Algorithm{..} <- getInputWithSqlCompletion
        AlgorithmName UniqAlgorithm "Algorithm Name"

    let args = ["list","implementations","-a",T.unpack algorithmName]
    implName <- withProcessCompletion args $ getInput "Implementation Name"

    prettyName <- getOptionalInput "Implementation Pretty Name"
    flags <- getOptionalInput "Flags"

    algoType <- getReadInput "Algorithm type"
    runnable <- getReadInput "Runnable"

    liftSql . Sql.insert_ $
        Implementation algoId implName prettyName flags algoType runnable

addVariant :: Input SqlM ()
addVariant = do
    Entity algoId _ <- getInputWithSqlCompletion
        AlgorithmName UniqAlgorithm "Algorithm Name"

    variantName <- getInput "Variant Name"
    flags <- getOptionalInput "Flags"

    let mkVariant gId = Variant gId algoId variantName flags Nothing False 0

    liftSql . runConduit $
        Sql.selectKeys [] []
        .| C.mapM_ (Sql.insert_ . mkVariant)

importResults :: Input SqlM ()
importResults = do
    Entity gpuId _ <- getInputWithSqlCompletion
        GPUName UniqGPU "GPU Name"

    Entity algoId _ <- getInputWithSqlCompletion
        AlgorithmName UniqAlgorithm "Algorithm Name"

    Entity implId _ <- getInputWithSqlCompletion
        ImplementationName (UniqImpl algoId) "Implementation Name"

    filepath <- withCompletion FileCompletion $
        getInputWith checkExists "Non-existent file!" "Result File"

    timestamp <- liftIO getCurrentTime
    liftSql . runConduit $
        C.sourceFile filepath
        .| C.decode C.utf8
        .| C.map (T.replace "," "")
        .| conduitParse externalResult
        .| C.mapM_ (insertResult gpuId algoId implId timestamp)
  where
    checkExists :: MonadIO m => Text -> m (Maybe FilePath)
    checkExists txt = bool Nothing (Just path) <$> liftIO (doesFileExist path)
      where
        path = T.unpack txt

    insertResult
        :: Key GPU
        -> Key Algorithm
        -> Key Implementation
        -> UTCTime
        -> ExternalResult
        -> SqlM ()
    insertResult gpuId algoId implId ts (Result gname varName Timing{..}) = do
        [graphId] <- logIfFail "More than one graph found for" gname $
            Sql.selectKeysList [GraphName ==. gname] []

        let uniqVariant = UniqVariant graphId algoId variantName

        Just varId <- logIfFail "No variant found" variantName $
            fmap entityKey <$> Sql.getBy uniqVariant

        Sql.insert_ $
            TotalTimer gpuId varId implId name minTime avgTime maxTime stddev ts
      where
        --FIXME get from command
        variantName
            | varName == "0" = "default"
            | otherwise = "Root " <> varName

taskHandler :: Handler SqlM (Text, Key Variant, a, b, Text)
taskHandler = Handle handler
  where
    handler :: (Text, Key Variant, a, b, Text) -> SomeException -> SqlM Action
    handler (_, varId, _, _, _) exc
      | Just Timeout <- fromException exc = return Retry
      | otherwise = do
          retries <- variantRetryCount <$> Sql.getJust varId
          if retries >= 5
             then return Drop
             else Retry <$ Sql.update varId [ VariantRetryCount +=. 1 ]

runTask
    :: (MonadMask m, MonadIO m, MonadLogger m)
    => Process
    -> (Text, Key Variant, a, b, Text)
    -> m (Key Variant, a, b, Text)
runTask Process{..} (label, x, y, z, cmd) = handleError $ do
    logInfoN $ "Running: " <> cmd
    result <- liftIO $ T.hPutStrLn inHandle cmd >> T.hGetLine outHandle
    logInfoN $ "Finished: " <> cmd
    return (x, y, z, result)
  where
    tryRemoveFile path = do
        result <- try . liftIO $ removeFile path
        case result of
            Left (SomeException e) -> logErrorN . T.pack $ displayException e
            Right () -> return ()

    fileStem = T.unpack label
    handleError act = act `onError` do
        logErrorN ("Failed: " <> cmd)
        tryRemoveFile $ fileStem <.> "timings"
        tryRemoveFile $ fileStem <.> "output"
        tryRemoveFile $ fileStem <.> "log"


runBenchmarks :: Int -> Int -> Input SqlM ()
runBenchmarks numNodes numRuns = liftSql $ do
    -- FIXME hardcoded GPU
    withProcessPool numNodes (GPU "TitanX" Nothing) $ \procPool -> runConduit $
        propertyJobs
        .| parMapM taskHandler numNodes (withProcess procPool runTask)
        .| C.mapM_ processProperty

    gpus <- Sql.selectList [] []
    forM_ gpus $ \(Entity gpuId gpu) -> do
        withProcessPool numNodes gpu $ \procPool -> runConduit $
            timingJobs numRuns gpuId
            .| parMapM taskHandler numNodes (withProcess procPool runTask)
            .| C.mapM_ (processTiming gpuId)

resetRetries :: SqlM ()
resetRetries = Sql.updateWhere [] [VariantRetryCount =. 0]

resetProperties :: SqlM ()
resetProperties = do
    Sql.deleteWhere ([] :: [Filter GraphProp])
    Sql.deleteWhere ([] :: [Filter StepProp])

resetMeasurements :: SqlM ()
resetMeasurements = do
    Sql.deleteWhere ([] :: [Filter StepTimer])
    Sql.deleteWhere ([] :: [Filter TotalTimer])

resetModels :: SqlM ()
resetModels = do
    Sql.deleteWhere ([] :: [Filter PredictionModel])
    Sql.deleteWhere ([] :: [Filter ModelGraphProperty])
    Sql.deleteWhere ([] :: [Filter ModelStepProperty])
    Sql.deleteWhere ([] :: [Filter UnknownPrediction])
    Sql.deleteWhere ([] :: [Filter UnknownSet])

commands :: String -> (InfoMod a, Parser (Input SqlM ()))
commands name = (,) docs . hsubparser $ mconcat
    [ subCommand "add-gpu" "register a new GPU"
        "Register a new GPU in the database." $ pure addGPU
    , subCommand "add-graphs" "register graphs"
        "Register new graphs in the database." $
        liftSql . addGraphs <$> some (strArgument graphFile)
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
                 pure $ liftSql resetRetries
    , subCommand "reset-properties" "deletes logged properties"
                 "Deletes the properties stored for each variant" $
                 pure $ liftSql resetProperties
    , subCommand "reset-measurements" "deletes timing measurements"
                 "Delete all timing measurements" $
                 pure $ liftSql resetMeasurements
    , subCommand "reset-models" "deletes models"
                 "Delete all stored models" $
                 pure $ liftSql resetModels
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

dynCompleter :: CompletionFunc (ReaderT (Completer SqlM) SqlM)
dynCompleter completeArgs = do
    ask >>= \case
        SimpleCompletion f -> lift $ completeWord Nothing " \t" f completeArgs
        FileCompletion -> completeFilename completeArgs

main :: IO ()
main = runSqlM commands $ (`runReaderT` emptyCompletion) . runInputT settings
  where
    emptyCompletion = SimpleCompletion $ const (return [])
    settings = setComplete dynCompleter defaultSettings
