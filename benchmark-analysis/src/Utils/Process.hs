{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Utils.Process
    ( CreateProcess
    , Process.proc
    , ReadWrite(..)
    , UnexpectedTermination(..)
    , readStdout
    , runProcess
    , runProcessCreation
    , unexpectedTermination
    , withPipe
    , withProcess
    , withStdin
    ) where

import Control.Monad (unless)
import Control.Monad.Catch (MonadMask, MonadThrow, SomeException(..))
import qualified Control.Monad.Catch as Except
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN, logWarnN)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Cont (ContT(..), evalContT, mapContT)
import Control.Monad.Reader (ReaderT, runReaderT, local, ask)
import Control.Monad.Trans.Resource (MonadResource, register, release)
import Data.Conduit.Process
    (ClosedStream(..), CmdSpec(..), StreamingProcessHandle)
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

import Exceptions

data ReadWrite = Read | Write deriving (Eq, Show)

newtype ProcessCreation r m a = ProcessCreation (ContT r (ReaderT (m ()) m) a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadLogger)

instance MonadTrans (ProcessCreation r) where
    lift = ProcessCreation . lift . lift

runProcessCreation :: Monad m => ProcessCreation r m r -> m r
runProcessCreation (ProcessCreation act) =
  runReaderT (evalContT act) $ return ()

withProcess
    :: ( InputSource stdin
       , OutputSink stdout
       , MonadIO m
       , MonadLogger m
       , MonadMask m
       )
    => CreateProcess
    -> (stdin -> stdout -> m r)
    -> ProcessCreation r m r
withProcess cp f = ProcessCreation . lift $
  Except.bracket alloc cleanup $ \(stdin, stdout, stderr, sph) -> do
    ask >>= \free -> lift $ do
        free
        (r, err) <- work stdin stdout stderr `Except.onException` terminate sph
        Process.waitForStreamingProcess sph >>= \case
            ExitSuccess -> r <$ logStderrOutput err
            ec -> logErrorN err >> throwUnexpected ec
  where
    logStderrOutput :: MonadLogger m => Text -> m ()
    logStderrOutput t = unless (T.null (T.strip t)) $ logWarnN t

    alloc = Process.streamingProcess cp
    cleanup (_, _, _, sph) = Process.closeStreamingProcessHandle sph

    terminate :: MonadIO m => StreamingProcessHandle -> m ()
    terminate =
      liftIO . Process.terminateProcess . Process.streamingProcessHandleRaw

    throwUnexpected :: MonadThrow m => ExitCode -> m a
    throwUnexpected =
      Except.throwM . UnexpectedTermination . ProcessExitedUnsuccessfully cp

    logAndRethrow
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => Handle -> SomeException -> m a
    logAndRethrow stderr (SomeException exc) = do
        err <- liftIO $ T.hGetContents stderr
        logErrorN err
        Except.throwM exc

    work stdin stdout stderr = Except.handle (logAndRethrow stderr) $
        (,) <$> f stdin stdout <*> liftIO (T.hGetContents stderr)

withProcess_
    :: ( InputSource stdin
       , OutputSink stdout
       , MonadIO m
       , MonadLogger m
       , MonadMask m
       )
    => CreateProcess
    -> (stdin -> stdout -> m r)
    -> m r
withProcess_ cp f = runProcessCreation $ withProcess cp f

runProcess
    :: (MonadIO m, MonadLogger m, MonadMask m) => FilePath -> [String] -> m ()
runProcess exe args = withProcess_ process $ \ClosedStream stdout -> do
    info <- liftIO $ T.hGetContents stdout
    unless (T.null info) $ logInfoN info
  where
    process = Process.proc exe args

readStdout
    :: (MonadIO m, MonadLogger m, MonadMask m) => CreateProcess -> m Text
readStdout process = withProcess_ process $ \ClosedStream ->
    liftIO . T.hGetContents

withStdin
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => CreateProcess -> (Handle -> m ()) -> m ()
withStdin process work = withProcess_ process $ \stdin stdout -> do
    work stdin
    liftIO $ hClose stdin
    info <- liftIO $ T.hGetContents stdout
    unless (T.null info) $ logInfoN info

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
        prettyCmd = case cmdspec p of
            ShellCommand s -> Pretty.reflow $ T.pack s
            RawCommand exe args -> mconcat
                [ "Executable:" <+> pretty exe
                , "Arguments:" <+> renderList args
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
    toException = toRuntimeError
    fromException = fromRuntimeError
    displayException = show . pretty
