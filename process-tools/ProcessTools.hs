{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ProcessTools
    ( ClosedStream(..)
    , CreateProcess
    , ExitCode(..)
    , Inherited(..)
    , Process.proc
    , ReadWrite(..)
    , UnexpectedTermination(..)
    , readStdout
    , readStdout_
    , runProcess
    , runProcess_
    , runProcessCreation
    , runProcessCreation_
    , unexpectedTermination
    , withPipe
    , withProcess
    , withStdin
    ) where

import Control.Monad (unless, void)
import Control.Monad.Catch
    (Exception(..), MonadMask, MonadThrow, SomeException(..))
import qualified Control.Monad.Catch as Except
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger, logErrorNS, logInfoNS, logWarnNS)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Cont (ContT(..), mapContT)
import Control.Monad.Reader (ReaderT, runReaderT, local, ask)
import Control.Monad.Trans.Resource (MonadResource, register, release)
import Data.Conduit.Process
    (ClosedStream(..), CmdSpec(..), Inherited(..), StreamingProcessHandle)
import qualified Data.Conduit.Process as Process
import Data.Streaming.Process
    (InputSource, OutputSink, ProcessExitedUnsuccessfully(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc (Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Util as Pretty
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Process (CreateProcess, cmdspec, cwd, env)

data ReadWrite = Read | Write deriving (Eq, Show)

data ProcessResult r = ProcResult
    { resultProcessCreation :: CreateProcess
    , resultExitCode :: ExitCode
    , resultValue :: r
    }

newtype ProcessCreation r m a = ProcessCreation (ContT r (ReaderT (m ()) m) a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadLogger)

instance MonadTrans (ProcessCreation r) where
    lift = ProcessCreation . lift . lift

runProcessCreation
    :: Monad m
    => ProcessCreation (ExitCode, r) m (ProcessResult r)
    -> m (ExitCode, r)
runProcessCreation (ProcessCreation act) =
    runReaderT (runContT act unwrap) $ return ()
  where
    unwrap :: Monad m => ProcessResult r -> m (ExitCode, r)
    unwrap ProcResult{..} = return (resultExitCode, resultValue)

runProcessCreation_
    :: MonadThrow m => ProcessCreation r m (ProcessResult r) -> m r
runProcessCreation_ (ProcessCreation act) =
    runReaderT (runContT act unwrap) $ return ()
  where
    unwrap :: MonadThrow m => ProcessResult r -> m r
    unwrap ProcResult{..} = case resultExitCode of
        ExitSuccess -> return resultValue
        _ -> throwUnexpected resultProcessCreation resultExitCode

    throwUnexpected :: MonadThrow m => CreateProcess -> ExitCode -> m a
    throwUnexpected cp =
      Except.throwM . UnexpectedTermination . ProcessExitedUnsuccessfully cp

withProcess
    :: ( InputSource stdin
       , OutputSink stdout
       , MonadIO m
       , MonadLogger m
       , MonadMask m
       )
    => CreateProcess
    -> (stdin -> stdout -> m a)
    -> ProcessCreation r m (ProcessResult a)
withProcess cp f = ProcessCreation . lift $
  Except.bracket alloc cleanup $ \(stdin, stdout, stderr, sph) -> do
    ask >>= \free -> lift $ do
        free
        (r, err) <- work stdin stdout stderr `Except.onException` terminate sph
        ec <- Process.waitForStreamingProcess sph
        ProcResult cp ec r <$ logStderrOutput err
  where
    logStderrOutput :: MonadLogger m => Text -> m ()
    logStderrOutput t = unless (T.null (T.strip t)) $
        logWarnNS "ProcessTools#logStderrOutput" t

    alloc = Process.streamingProcess cp
    cleanup (_, _, _, sph) = Process.closeStreamingProcessHandle sph

    terminate :: MonadIO m => StreamingProcessHandle -> m ()
    terminate =
      liftIO . Process.terminateProcess . Process.streamingProcessHandleRaw

    logAndRethrow
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => Handle -> SomeException -> m a
    logAndRethrow stderr (SomeException exc) = do
        err <- liftIO $ T.hGetContents stderr
        logErrorNS "ProcessTools#logAndRethrow" err
        Except.throwM exc

    work stdin stdout stderr = Except.handle (logAndRethrow stderr) $
        (,) <$> f stdin stdout <*> liftIO (T.hGetContents stderr)

runProcess_
    :: (MonadIO m, MonadLogger m, MonadMask m) => FilePath -> [String] -> m ()
runProcess_ exe args = void $ runProcess exe args

runProcess
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => FilePath -> [String] -> m ExitCode
runProcess exe args = do
    (ec, ()) <- runProcessCreation $ do
        withProcess process $ \ClosedStream stdout -> do
            info <- liftIO $ T.hGetContents stdout
            unless (T.null (T.stripStart info)) $
                logInfoNS "ProcessTools#runProcess" info
    return ec
  where
    process = Process.proc exe args

readStdout
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => CreateProcess -> m (ExitCode, Text)
readStdout process = runProcessCreation $ do
    withProcess process $ \ClosedStream -> liftIO . T.hGetContents

readStdout_
    :: (MonadIO m, MonadLogger m, MonadMask m) => CreateProcess -> m Text
readStdout_ process = runProcessCreation_ $ do
    withProcess process $ \ClosedStream -> liftIO . T.hGetContents

withStdin
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => CreateProcess -> (Handle -> m ()) -> m ()
withStdin process work = runProcessCreation_ $ do
    withProcess process $ \stdin stdout -> do
        work stdin
        liftIO $ hClose stdin
        info <- liftIO $ T.hGetContents stdout
        unless (T.null info) $ logInfoNS "ProcessTools#withStdin" info

withPipe
    :: (MonadMask m, MonadResource m)
    => ReadWrite -> ProcessCreation r m (String, Handle)
withPipe rw = ProcessCreation $ do
    ((fd, key), (handle, _)) <- ContT $ Except.bracket allocate cleanup
    mapContT (local (>> release key)) $ return (show fd, handle)
  where
    readOrWrite = case rw of
        Read -> swap
        Write -> id

    allocate = do
        (remoteFd, localFd) <- readOrWrite <$> liftIO createPipe
        localHandle <- liftIO $ fdToHandle localFd
        hndKey <- register $ hClose localHandle
        fdKey <- register $ closeFd remoteFd
        return ((remoteFd, fdKey), (localHandle, hndKey))

    cleanup ((_, key1), (_, key2)) = release key1 >> release key2

data UnexpectedTermination = UnexpectedTermination ProcessExitedUnsuccessfully
    deriving (Show, Typeable)

unexpectedTermination :: CreateProcess -> ExitCode -> UnexpectedTermination
unexpectedTermination p = UnexpectedTermination . ProcessExitedUnsuccessfully p

instance Pretty UnexpectedTermination where
    pretty (UnexpectedTermination (ProcessExitedUnsuccessfully p exitCode)) =
        Pretty.group $ mconcat
            [ Pretty.reflow "Unexpected termination of child process:"
            , Pretty.line
            , prettyCmd
            , Pretty.line
            , "Exit code:" <+> Pretty.viaShow exitCode
            , Pretty.line
            , prettyCwd
            , prettyEnv
            ]
      where
        renderPrettyList = Pretty.align . Pretty.encloseSep "[" "]" ", "
        prettyCmd = case cmdspec p of
            ShellCommand s -> Pretty.reflow $ T.pack s
            RawCommand exe args -> mconcat
                [ "Executable:" <+> pretty exe
                , Pretty.line
                , "Arguments:" <+> renderPrettyList (map pretty args)
                ]

        prettyCwd = case cwd p of
            Nothing -> mempty
            Just path -> "Working dir:" <> Pretty.softline <> pretty path

        prettyEnv = case env p of
            Nothing -> mempty
            Just environment -> mconcat
                [ "Environment:"
                , Pretty.line
                , Pretty.indent 4 . Pretty.vsep $ map prettyEnvVar environment
                ]

        prettyEnvVar (var, value) = pretty var <+> "=" <+> pretty value

instance Exception UnexpectedTermination where
    displayException = show . pretty
