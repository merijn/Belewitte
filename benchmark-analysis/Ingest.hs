{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Concurrent hiding (yield)
import Control.Monad (replicateM_, void, when)
import Control.Monad.Catch (MonadMask, MonadThrow, catchAll, mask_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger
    ( MonadLogger, LogSource, LogLevel(..), filterLogger, logErrorN
    , runChanLoggingT, runStderrLoggingT, unChanLoggingT)
import Data.Attoparsec.Text
    (Parser, char, decimal, double, endOfLine, takeTill)
import Data.Conduit (Consumer, Producer, (.|), ($$), runConduit)
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.Combinators as C
import Data.Conduit.List (sourceList)
import Data.Conduit.Process
    (ClosedStream(..), CreateProcess, proc, withCheckedProcessCleanup)
import Data.Conduit.Text (decode, utf8)
import Data.List.Split (splitOneOf)
import Data.IORef (newIORef, atomicModifyIORef')
import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.Persist.Sqlite
import GHC.Conc.Sync (getNumProcessors)
import Options.Applicative
    ( ParserInfo, execParser, fullDesc, header, helper, info, progDesc
    , strArgument)
import qualified Options.Applicative as Optparse
import System.FilePath
    (dropExtension, splitExtension, takeExtension, takeFileName)
import Text.Read (readMaybe)

import BroadcastChan
import Schema

data Ingest = Files [FilePath] | Dir FilePath
    deriving (Eq, Show)

data Import =
  Import
    { tag :: Text
    , graphs :: Ingest
    , properties :: Ingest
    , timings :: Ingest
    }
    deriving (Eq, Show)

data Task
    = GraphTask FilePath
    | TimingTask FilePath
    | PropertyTask FilePath
    deriving (Eq, Show)

data Timing
  = Timing
    { graph :: Text
    , variant :: Text
    , timer :: Text
    , minTime :: Double
    , avgTime :: Double
    , maxTime :: Double
    , stddev :: Double
    } deriving (Show, Eq)

checkedSourceProcessWithConsumer
    :: (MonadIO m, MonadMask m)
    => CreateProcess
    -> Consumer ByteString m ()
    -> m ()
checkedSourceProcessWithConsumer cp consumer =
    withCheckedProcessCleanup cp $ handleStdout
  where
    handleStdout ClosedStream (source, close) ClosedStream = do
        source $$ consumer
        close

runParallel
    :: (MonadLogger m, MonadIO m)
    => Int
    -> (a -> IO ())
    -> Consumer a m ()
runParallel i f = do
    (chan, sem, done) <- liftIO $ do
        inChanIn <- newBroadcastChan
        inChanOut <- newBChanListener inChanIn
        done <- newEmptyMVar
        sem <- newQSem i
        ref <- newIORef i

        let decrement :: IO Int
            decrement = atomicModifyIORef' ref (\x -> ((x-1), (x-1)))

            processInput :: (forall a . IO a -> IO a) -> IO ()
            processInput restore = do
                x <- readBChan inChanOut
                case x of
                    Nothing -> do
                        j <- decrement
                        when (j == 0) $ putMVar done ()
                    Just a -> do
                        signalQSem sem
                        restore (f a) `catchAll` const (return ())
                        processInput restore
        mask_ . replicateM_ i $ forkIOWithUnmask processInput
        return (inChanIn, sem, done)
    C.mapM_ $ \a -> liftIO $ do
        waitQSem sem
        void (writeBChan chan a)
    liftIO $ do
        closeBChan chan
        readMVar done

textField :: Parser Text
textField = takeTill (\c -> c == ':' || c == '\n') <* char ':'

property :: Parser (Text, Double)
property = (,) <$> textField <*> double <* endOfLine

stepProperty :: Parser (Int, Double)
stepProperty = (,) <$> decimal <* char '\t' <*> double <* endOfLine

timing :: Parser Timing
timing = do
    graph <- textField
    variant <- textField
    timer <- textField
    minTime <- double <* char ' '
    avgTime <- double <* char ' '
    maxTime <- double <* char ' '
    stddev <- double <* endOfLine
    return Timing{..}

conduitParse
    :: (MonadThrow m, MonadLogger m)
    => Parser a
    -> (a -> m ())
    -> Consumer Text m ()
conduitParse parser fun = conduitParser parser .| C.mapM_ (fun . snd)

loadGraphProperties :: Text -> FilePath -> SqlTx ()
loadGraphProperties graphtag file = do
    k <- getUniq $ Graph name graphtag Nothing

    let insertProp :: (Text, Double) -> SqlTx ()
        insertProp (prop, val) = insert_ $ GraphProp k prop val

    withLoggedExceptions (file ++ ": ") $
        checkedSourceProcessWithConsumer process $
            decode utf8 .| conduitParse property insertProp
  where
    process :: CreateProcess
    process = proc  "./graph-details" [file]

    name :: Text
    name = T.pack . dropExtension . takeFileName $ file

loadTimings :: Text -> FilePath -> SqlTx ()
loadTimings graphtag file = case T.pack <$> splitOneOf "." desc of
    [algo,impl,gpu] -> do
        algoId <- getUniq $ Algorithm algo Nothing
        implId <- getUniq $ Implementation algoId impl Nothing
        gpuId <- getUniq $ GPU gpu

        let totalTime = TotalTime gpuId implId
            stepTime = StepTime gpuId implId

            insertTiming :: Timing -> SqlTx ()
            insertTiming Timing{..} = do
                graphId <- getUniq $ Graph graph graphtag Nothing
                varId <- getUniq $ Variant graphId algoId variant
                case timerType timer of
                    -- FIXME: should only run once?
                    Left t -> insert_ $
                        totalTime varId t minTime avgTime maxTime stddev
                    Right i -> insert_ $
                        stepTime varId i minTime avgTime maxTime stddev

        runConduit . withLoggedExceptions (file ++ ": ") $
            C.sourceFile file .| decode utf8 .| conduitParse timing insertTiming
    _ -> logErrorN . T.pack $ "File name doesn't match format: " ++ file
  where
    desc :: String
    desc = dropExtension $ takeFileName file

    timerType :: Text -> Either Text Int
    timerType timer = maybe (Left timer) Right $ do
        result <- "bfsLevel" `T.stripPrefix` timer
        readMaybe $ T.unpack result

loadProperties :: Text -> FilePath -> SqlTx ()
loadProperties graphtag file = case T.pack <$> splitOneOf "." desc of
    [graph, variant, algo, _impl, _gpu] -> do
        varId <- do
            graphId <- getUniq $ Graph graph graphtag Nothing
            algoId <- getUniq $ Algorithm algo Nothing
            getUniq $ Variant graphId algoId variant

        let insertProp :: (Int, Double) -> SqlTx ()
            insertProp (step, v) = insertUniq $ StepProp varId step propName v

        runConduit . withLoggedExceptions (file ++ ": ") $
            C.sourceFile file
            .| decode utf8
            .| C.map (T.replace "," "")
            .| conduitParse stepProperty insertProp
    _ -> logErrorN . T.pack $ "File name doesn't match format: " ++ file
  where
    (desc, '.':(T.pack -> propName)) = splitExtension $ takeFileName file

generateTasks :: MonadResource m => Import -> Producer m Task
generateTasks Import{..} = do
    filteredFiles graphs GraphTask $ checkExtension ".graph"
    filteredFiles properties PropertyTask $ not . checkExtension ".timings"
    filteredFiles timings TimingTask $ checkExtension ".timings"
  where
    yieldFiles :: (MonadIO m, MonadResource m) => Ingest -> Producer m FilePath
    yieldFiles (Files fps) = sourceList fps
    yieldFiles (Dir dir) = C.sourceDirectoryDeep False dir

    checkExtension :: String -> FilePath -> Bool
    checkExtension ext = (ext==) . takeExtension

    filteredFiles
        :: MonadResource m
        => Ingest
        -> (FilePath -> Task)
        -> (FilePath -> Bool)
        -> Producer m Task
    filteredFiles input task predicate =
        yieldFiles input .| C.filter predicate .| C.map task

taskToSql :: Text -> Task -> SqlTx ()
taskToSql graphtag (GraphTask file) = loadGraphProperties graphtag file
taskToSql graphtag (TimingTask file) = loadTimings graphtag file
taskToSql graphtag (PropertyTask file) = loadProperties graphtag file

programOpts :: ParserInfo Import
programOpts = info (helper <*> config)
    ( fullDesc
    <> progDesc "Read benchmark data into SQLite."
    <> header "Some header" )
  where
    config :: Optparse.Parser Import
    config = (\x y -> Import "" (Dir x) (Dir y) (Dir y))
            <$> strArgument mempty
            <*> strArgument mempty

main :: IO ()
main = do
    numCPUs <- getNumProcessors
    setNumCapabilities numCPUs

    cmd@Import{tag} <- execParser programOpts

    logChan <- newChan
    forkIO . runStderrLoggingT . filterLogger isNotDebug $
        unChanLoggingT logChan

    runChanLoggingT logChan . runWithDb "test.db" numCPUs $ do
        runSql $ runMigrationSilent migrateAll
        pool <- ask

        let runTask :: Text -> Task -> IO ()
            runTask t task = runLoggedResource $ runReaderT work pool
              where
                work :: SqlM ()
                work = runSql $ taskToSql t task

                runLoggedResource :: ResourceT (LoggingT IO) a -> IO a
                runLoggedResource = runChanLoggingT logChan . runResourceT

        runConduit $ generateTasks cmd .| runParallel numCPUs (runTask tag)
  where
    isNotDebug :: LogSource -> LogLevel -> Bool
    isNotDebug _ LevelDebug = False
    isNotDebug _ _ = True

    runWithDb :: Text -> Int -> SqlM a -> LoggingT IO a
    runWithDb path n = runResourceT . withSqlitePool path n . runReaderT
