{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema
    ( ImplType(..)
    , LogLevel(..)
    , LogSource
    , MonadIO(liftIO)
    , ReaderT(..)
    , Text
    , module Schema
    ) where

import Control.Exception (Exception(displayException))
import Control.Monad.Catch
    (MonadCatch, MonadMask, MonadThrow, SomeException(..), catch, throwM)
import Control.Monad.IO.Unlift
    (MonadIO(liftIO), MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Logger (LoggingT, LogLevel, LogSource, MonadLogger)
import qualified Control.Monad.Logger as Log
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Conduit (ConduitT)
import Database.Persist.Quasi
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text)
import qualified Data.Text as T

import ImplType (ImplType(..))

type SqlM = ReaderT (RawSqlite SqlBackend) BaseM

newtype BaseM a = BaseM { runBaseM :: ResourceT (LoggingT IO) a }
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadLogger, MonadMask
  , MonadResource, MonadThrow)

instance MonadUnliftIO BaseM where
  askUnliftIO = BaseM $ withUnliftIO $ \u ->
                            return (UnliftIO (unliftIO u . runBaseM))

type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))

runSqlM :: (LogSource -> LogLevel -> Bool) -> Text -> SqlM a -> IO a
runSqlM logFilter path =
   runLog . runBaseRes . withRawSqliteConnInfo connInfo . runReaderT
  where
    runLog :: LoggingT IO a -> IO a
    runLog = Log.runStderrLoggingT . Log.filterLogger logFilter

    runBaseRes :: BaseM a -> LoggingT IO a
    runBaseRes = runResourceT . runBaseM

    connInfo :: SqliteConnectionInfo
    connInfo = mkSqliteConnectionInfo path

withLoggedExceptions :: (MonadMask m, MonadLogger m) => String -> m r -> m r
withLoggedExceptions msg act = act `catch`  \(SomeException e) -> do
    Log.logErrorN . T.pack $ msg ++ displayException e
    throwM e

showSqlKey :: ToBackendKey SqlBackend record => Key record -> Text
showSqlKey = T.pack . show . fromSqlKey

whenNotExists
    :: SqlRecord record
    => [Filter record] -> ConduitT i a SqlM () -> ConduitT i a SqlM ()
whenNotExists filters act = lift (selectFirst filters []) >>= \case
    Just _ -> return ()
    Nothing -> act

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
