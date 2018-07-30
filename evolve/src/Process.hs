{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process
    ( ThreadState
    , Cleanup(..)
    , CleanupActions(Cleanup, parent, child)
    , ignoreCleanup
    , throwToWeak
    , killWeak
    , withChildrenDo
    , waitForChildren
    , killChildren
    , numChildren
    , fork
    , forkTwinEffects
    , Task(..)
    , Result(..)
    , Reply(..)
    , ExecutionError(..)
    , lookupAddr
    , recv
    , fromInput
    , fromFiniteInput
    , send
    , toOutput
    , withSocket
    , ProcFailed(..)
    , withProcess
    , Text
    , module Control.Concurrent
    , atomically
    , module Control.Concurrent.STM
    , module Network.Socket
    , module Pipes
    , module Pipes.Concurrent
    , module Pipes.Core
    , module Pipes.Safe
    , module System.Process
    ) where

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM hiding (atomically)
import qualified Control.Concurrent.STM as STM
import Control.Exception (AsyncException(ThreadKilled))
import Control.Monad (forever, unless)
import qualified Control.Monad.Catch as Catch
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Typeable (Typeable)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import GHC.Conc (getUncaughtExceptionHandler,setUncaughtExceptionHandler)
import Network.Socket hiding (recv, send)
import qualified Network.Socket.ByteString as BS (recv, sendAll)
import Pipes
import Pipes.Concurrent hiding (recv, send, atomically, fromInput, toOutput)
import qualified Pipes.Concurrent as P
import Pipes.Core
import Pipes.Binary
import Pipes.Parse
import Pipes.Safe
import System.Exit (ExitCode)
import System.IO (Handle, hClose, hPutStrLn, stderr)
import System.Mem.Weak (Weak, deRefWeak)
import System.Process (CmdSpec(..), cmdspec, StdStream(..), CreateProcess
                      , createProcess, proc, ProcessHandle, std_out, std_err
                      , terminateProcess, waitForProcess)

import qualified Pipes.Prelude as P

atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically

data ThreadState
    = ThreadState
    { parentTid :: Weak ThreadId
    , childTids :: TVar (Map String (Weak ThreadId))
    }

data CleanupActions
    = Custom (IO ()) CleanupActions
    | Cleanup { parent :: Cleanup, child :: Cleanup }

data Cleanup = Ignore | Reraise | Kill

data Kill = KillChild | KillTwin deriving (Show,Typeable)
instance Exception Kill

ignoreCleanup :: CleanupActions
ignoreCleanup = Cleanup{ parent = Ignore, child = Ignore }

throwToWeak :: Exception a => a -> Weak ThreadId -> IO ()
throwToWeak e wTid = do
    deRefWeak wTid >>= \case
        Just tid -> throwTo tid e
        Nothing -> return ()

killWeak :: Weak ThreadId -> IO ()
killWeak = throwToWeak ThreadKilled

mapChildren
    :: (?threadState :: ThreadState) => (Weak ThreadId -> IO ()) -> IO ()
mapChildren f = readTVarIO (childTids ?threadState) >>= mapM_ f

withChildrenDo
    :: forall a . Cleanup
    -> ((?threadState :: ThreadState) => IO a)
    -> IO a
withChildrenDo cleanup body = do
    handler <- getUncaughtExceptionHandler
    setUncaughtExceptionHandler $ \e -> do
        case fromException e of
            Just KillTwin -> return ()
            Just KillChild -> return ()
            Nothing -> handler e

    tid <- myThreadId >>= mkWeakThreadId
    tvar <- newTVarIO M.empty
    let ?threadState = ThreadState { parentTid = tid, childTids = tvar } in act
  where
    act :: (?threadState :: ThreadState) => IO a
    act = mask $ \restore -> do
        result <- Catch.try (restore body)
        case cleanup of
            Kill -> mapChildren killWeak
            Reraise | Left (SomeException e) <- result -> mapChildren (throwToWeak e)
            _ -> return ()

        either throwM return result

withChildren :: ((?threadState :: ThreadState) => IO a) -> IO a
withChildren = withChildrenDo Ignore

waitForChildren :: (?threadState :: ThreadState) => IO ()
waitForChildren =
  atomically $ readTVar (childTids ?threadState) >>= check . M.null

killChildren :: (?threadState :: ThreadState) => IO ()
killChildren = mapChildren $ throwToWeak KillChild

numChildren :: (?threadState :: ThreadState) => IO Int
numChildren = M.size <$> readTVarIO (childTids ?threadState)

handleCleanup
    :: (?threadState :: ThreadState)
    => CleanupActions
    -> Either SomeException ()
    -> IO ()
handleCleanup (Custom act cleanup) result = act >> handleCleanup cleanup result
handleCleanup cleanup result = do
    case child cleanup of
        Kill -> mapChildren killWeak
        Reraise | Left e <- result -> case fromException e of
            Just KillTwin -> mapChildren $ throwToWeak KillChild
            _ -> mapChildren $ throwToWeak e
        _ -> return ()

    case parent cleanup of
        Kill -> killWeak (parentTid ?threadState)
        Reraise | Left e <- result -> case fromException e of
            Just KillTwin -> return ()
            Just KillChild -> return ()
            Nothing -> throwToWeak e (parentTid ?threadState)
        _ -> return ()

fork
    :: (?threadState :: ThreadState, MonadIO m)
    => CleanupActions
    -> ((?threadState :: ThreadState) => SafeT IO ())
    -> m (Weak ThreadId)
fork cleanup act = liftIO $ mask $ \restore -> do
    mvar <- newEmptyMVar
    tid <- forkFinally (takeMVar mvar >> restore (withChildren (runSafeT act)))
                       (handleCleanup $ Custom unregister cleanup)

    weakTid <- mkWeakThreadId tid
    updateTVarIO $ M.insert (show tid) weakTid

    putMVar mvar ()
    return weakTid
  where
    unregister = myThreadId >>= updateTVarIO . M.delete . show
    updateTVarIO = atomically . modifyTVar' (childTids ?threadState)

forkTwinEffects
    :: (?threadState :: ThreadState)
    => CleanupActions
    -> ((?threadState :: ThreadState) => Effect (SafeT IO) ())
    -> ((?threadState :: ThreadState) => Effect (SafeT IO) ())
    -> IO (IO ())
forkTwinEffects cleanup act1 act2 = do
    rec
        tid1 <- fork (Custom (throwToWeak KillTwin tid2) cleanup)
                     (runEffect act1)
        tid2 <- fork (Custom (throwToWeak KillTwin tid1) cleanup)
                     (runEffect act2)
    return $ killWeak tid1

type Id = Word
data Task = Task Id [String] deriving (Generic,Show)
data Result = Failed String | Success Text deriving (Generic,Show)
data Reply = Request Int | Result Id Result deriving (Generic,Show)
data Error = InputDisconnected | DecodeError | SinkExhausted | SourceExhausted
    deriving (Show, Typeable)

instance Exception Error

data ExecutionError = ExecutionFailed String deriving (Show, Typeable)
instance Exception ExecutionError

instance Binary Task
instance Binary Result
instance Binary Reply

lookupAddr :: HostName -> PortNumber -> IO [SockAddr]
lookupAddr host port =
  map addrAddress <$> getAddrInfo (Just hints) (Just host) (Just (show port))
  where
    hints = defaultHints { addrFlags = [AI_NUMERICSERV] }

recv :: (MonadThrow m, MonadIO m) => Input a -> m a
recv input = do
    result <- liftIO . atomically $ P.recv input
    case result of
        Just x -> return x
        Nothing -> throwM SourceExhausted

fromInput :: MonadSafe m => Input a -> Producer' a m r
fromInput input = do
    P.fromInput input
    throwM SourceExhausted

fromFiniteInput :: (MonadIO m) => Input a -> Producer' a m ()
fromFiniteInput = P.fromInput

send :: (MonadThrow m, MonadIO m) => Output a -> a -> m ()
send output x = do
    result <- liftIO . atomically $ P.send output x
    unless result $ throwM SinkExhausted

toOutput :: MonadSafe m => Output a -> Consumer' a m r
toOutput output = do
    P.toOutput output
    throwM SinkExhausted

finalize :: MonadSafe m => Effect m r -> Effect m r
finalize eff = finally eff (liftIO P.performGC)

withSocket
    :: (?threadState :: ThreadState, Binary a, Binary b, Show b)
    => CleanupActions
    -> Socket
    -> Input a
    -> Output b
    -> IO (IO ())
withSocket cleanup sock input output =
    forkTwinEffects (Custom (close sock) cleanup)
                    (finalize fromSocket)
                    (finalize toSocket)
  where
    socketReader :: MonadSafe m => Producer ByteString m r
    socketReader = forever $ do
        bs <- liftIO $ BS.recv sock 4096
        if BS.null bs
           then throwM InputDisconnected
           else yield bs

    parser :: (Binary b, MonadSafe m) => Producer b m r
    parser = do
        parsed decode socketReader
        throwM DecodeError

    fromSocket :: Effect (SafeT IO) r
    fromSocket = parser >-> toOutput output

    toSocket :: Effect (SafeT IO) ()
    toSocket = for (fromInput input) encode >-> P.mapM_ (liftIO . BS.sendAll sock)

type ProcHandles = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

data ProcFailed = ProcFailed String deriving (Show,Typeable)
instance Exception ProcFailed

withProcess
    :: forall e m b . (Exception e, MonadIO m, MonadMask m)
    => CreateProcess
    -> (Either e Text -> m b)
    -> m b
withProcess procDesc getResult = Catch.try (go 5) >>= getResult
  where
    process :: MonadIO m => m ProcHandles
    process = liftIO $ createProcess procDesc{ std_out = CreatePipe }

    showCmd :: CmdSpec -> String
    showCmd (ShellCommand s) = s
    showCmd (RawCommand exe args) = unwords (exe:args)

    go :: (MonadThrow m, MonadIO m) => Int -> m Text
    go 0 = throwM . ProcFailed $ showCmd (cmdspec procDesc)
    go n = do
        result <- Catch.try $ Catch.bracket process cleanup work
        case result of
            Right v -> return v
            Left e | Just ProcFailed{} <- fromException e -> throwM e
                   | Just KillTwin <- fromException e -> throwM e
                   | Just KillChild <- fromException e -> throwM e
                   | otherwise -> do
                        liftIO . hPutStrLn stderr $ show e
                        go (n-1)

    work :: ProcHandles -> m Text
    work (_, Nothing, _, _) = throwM $ ProcFailed "Stdout not open in child."
    work (_, Just hout, _, pid) = do
        result <- liftIO $ T.hGetContents hout
        liftIO $ waitForProcess pid
        return result

    cleanup :: ProcHandles -> m ExitCode
    cleanup (hin, hout, herr, pid) = liftIO $ do
        mapM_ hClose hin
        mapM_ hClose hout
        mapM_ hClose herr
        terminateProcess pid
        waitForProcess pid
