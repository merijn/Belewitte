{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module ProcessUtils
    ( CreateProcess
    , Process.proc
    , Process.withCheckedProcessCleanup
    , UnexpectedTermination(..)
    , readStdout
    , runProcess
    , unexpectedTermination
    , withStdin
    ) where

import Control.Monad (unless)
import Control.Monad.Catch (MonadMask, MonadThrow, SomeException(..))
import qualified Control.Monad.Catch as Except
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN, logWarnN)
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
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.IO (Handle)
import System.Process (CreateProcess, cmdspec, cwd, env)

import Exceptions

withCheckedProcess
    :: ( InputSource stdin
       , OutputSink stdout
       , MonadIO m
       , MonadLogger m
       , MonadMask m
       )
    => CreateProcess -> (stdin -> stdout -> m a) -> m a
withCheckedProcess cp f =
    Except.bracket alloc cleanup $ \(stdin, stdout, stderr, sph) -> do
        (r, err) <- work stdin stdout stderr `Except.onException` terminate sph
        Process.waitForStreamingProcess sph >>= \case
            ExitSuccess -> r <$ logWarnN err
            ec -> logErrorN err >> throwUnexpected ec
  where
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

runProcess
    :: (MonadIO m, MonadLogger m, MonadMask m) => FilePath -> [String] -> m ()
runProcess exe args = withCheckedProcess process $ \ClosedStream stdout -> do
    info <- liftIO $ T.hGetContents stdout
    unless (T.null info) $ logInfoN info
  where
    process = Process.proc exe args

readStdout
    :: (MonadIO m, MonadLogger m, MonadMask m) => CreateProcess -> m Text
readStdout process = withCheckedProcess process $ \ClosedStream ->
    liftIO . T.hGetContents

withStdin
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => CreateProcess -> (Handle -> m ()) -> m ()
withStdin process work = withCheckedProcess process $ \stdin stdout -> do
    work stdin
    info <- liftIO $ T.hGetContents stdout
    unless (T.null info) $ logInfoN info

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
