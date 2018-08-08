{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema
    ( Hash(..)
    , ImplType(..)
    , Int64
    , Key
    , fromSqlKey
    , toSqlKey
    , LogLevel(..)
    , LogSource
    , MonadIO(liftIO)
    , MonadLogger
    , MonadThrow
    , ReaderT(..)
    , Text
    , module Schema
    ) where

import Control.Exception (Exception(displayException))
import Control.Monad ((>=>), when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)
import Control.Monad.Fail
import Control.Monad.IO.Unlift
    (MonadIO(liftIO), MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Logger (LoggingT, LogLevel, LogSource, MonadLogger)
import qualified Control.Monad.Logger as Log
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit (ConduitT)
import Data.Proxy (Proxy(Proxy))
import Database.Persist.Quasi
import Database.Persist.Sqlite hiding (Connection)
import Database.Persist.TH
import Database.Sqlite.Internal
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Foreign (Ptr, FunPtr, nullPtr, nullFunPtr)
import Foreign.C (CInt(..), CString, withCString)
import qualified Lens.Micro.Extras as Lens

import Types (ImplType(..), Hash(..))

data Abort = Abort deriving (Show, Typeable)
instance Exception Abort where

data Error = Error Text deriving (Show)
instance Exception Error

type SqlM = ReaderT (RawSqlite SqlBackend) BaseM

type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))

newtype BaseM a =
  BaseM { runBaseM :: ReaderT (IORef Text) (ResourceT (LoggingT IO)) a }
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadLogger, MonadMask
  , MonadResource, MonadThrow)

showText :: Show a => a -> Text
showText = T.pack . show

logIfFail :: Show v => Text -> v -> SqlM a -> SqlM a
logIfFail tag val action = do
    lift . BaseM $ do
        ref <- ask
        liftIO . writeIORef ref $ tag <> ": " <> showText val
    action

instance MonadFail BaseM where
    fail _ = BaseM $ do
        ref <- ask
        msg <- liftIO $ readIORef ref
        Log.logErrorN msg
        throwM Abort

instance MonadUnliftIO BaseM where
  askUnliftIO = BaseM $ withUnliftIO $ \u ->
                            return (UnliftIO (unliftIO u . runBaseM))

foreign import ccall "sqlite-functions.h &randomFun"
    randomFun :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "random.h &vector_step"
    vector_step :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "random.h &vector_finalise"
    vector_finalise :: FunPtr (Ptr () -> IO ())

foreign import capi "sqlite3.h value SQLITE_ANY"
    sqliteAny :: CInt

foreign import capi "sqlite3.h value SQLITE_OK"
    sqliteOk :: CInt

foreign import ccall "sqlite3.h sqlite3_extended_errcode"
    getExtendedError :: Ptr () -> IO CInt

foreign import ccall "sqlite3.h sqlite3_errstr"
    getErrorString :: CInt -> IO CString

foreign import ccall "sqlite3.h sqlite3_create_function"
    createFun :: Ptr () -> CString -> CInt -> CInt -> Ptr ()
              -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
              -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
              -> FunPtr (Ptr () -> IO ())
              -> IO CInt

createSqliteFun
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => Ptr ()
    -> CInt
    -> String
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> IO ())
    -> m ()
createSqliteFun sqlPtr nArgs strName sqlFun aggStep aggFinal = do
    result <- liftIO . withCString strName $ \name -> do
        createFun sqlPtr name nArgs sqliteAny nullPtr sqlFun aggStep aggFinal
    when (result /= sqliteOk) $ do
        bs <- liftIO $ unpackError sqlPtr
        case decodeUtf8' bs of
            Left exc -> logThrowM exc
            Right txt -> Log.logErrorN txt
  where
    unpackError = getExtendedError >=> getErrorString >=> BS.packCString

createSqlFun
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => Ptr ()
    -> CInt
    -> String
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> m ()
createSqlFun sqlPtr nArgs name funPtr =
  createSqliteFun sqlPtr nArgs name funPtr nullFunPtr nullFunPtr

createSqlAggregate
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => Ptr ()
    -> CInt
    -> String
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> IO ())
    -> m ()
createSqlAggregate sqlPtr nArgs name =
  createSqliteFun sqlPtr nArgs name nullFunPtr

runSqlM :: (LogSource -> LogLevel -> Bool) -> Text -> SqlM a -> IO a
runSqlM logFilter path work = do
  ref <- newIORef ""
  runLog . runBaseRes ref . withRawSqliteConnInfo connInfo . runReaderT $ do
    sqlitePtr <- asks $ getSqlitePtr . Lens.view rawSqliteConnection
    createSqlFun sqlitePtr 1 "random" randomFun
    createSqlAggregate sqlitePtr 3 "vector" vector_step vector_finalise
    rawExecute "PRAGMA busy_timeout = 1000" []
    work
  where
    runLog :: LoggingT IO a -> IO a
    runLog = Log.runStderrLoggingT . Log.filterLogger logFilter

    runBaseRes :: IORef Text -> BaseM a -> LoggingT IO a
    runBaseRes ref = runResourceT . (`runReaderT` ref) . runBaseM

    connInfo :: SqliteConnectionInfo
    connInfo = mkSqliteConnectionInfo path

    getSqlitePtr :: Connection -> Ptr ()
    getSqlitePtr (Connection _ (Connection' ptr)) = ptr

logThrowM :: (Exception e, MonadLogger m, MonadThrow m) => e -> m r
logThrowM exc = do
    Log.logErrorN . T.pack . displayException $ exc
    throwM exc

showSqlKey :: ToBackendKey SqlBackend record => Key record -> Text
showSqlKey = showText . fromSqlKey

whenNotExists
    :: SqlRecord record
    => [Filter record] -> ConduitT i a SqlM () -> ConduitT i a SqlM ()
whenNotExists filters act = lift (selectFirst filters []) >>= \case
    Just _ -> return ()
    Nothing -> act

validateKey
    :: forall record
     . (ToBackendKey SqlBackend record, SqlRecord record)
    => Int64 -> SqlM (Key record)
validateKey n = get key >>= \case
    Just _ -> return key
    Nothing -> do
        let name = entityHaskell $ entityDef (Proxy :: Proxy record)
        Log.logErrorN . mconcat $
            [ "No ", unHaskellName name, " entry found for id: ", showText n ]
        throwM Abort
  where
    key = toSqlKey n

getUniq :: SqlRecord record => record -> SqlM (Key record)
getUniq record = do
    result <- getBy =<< onlyUnique record
    case result of
        Nothing -> insert record
        Just (Entity k _) -> return k

insertUniq
    :: (SqlRecord record, Eq record, Show record)
    => record -> SqlM ()
insertUniq record = do
    result <- insertBy record
    case result of
        Left (Entity _ r) | record /= r -> Log.logErrorN . T.pack $ mconcat
            ["Unique insert failed:\nFound: ", show r, "\nNew: ", show record]
        _ -> return ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings "src/schema")
