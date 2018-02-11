{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema
    ( ByteString
    , LoggingT
    , MonadIO(liftIO)
    , MonadResource
    , ReaderT(..)
    , ResourceT
    , Text
    , ask
    , runResourceT
    , module Schema
    ) where

import Control.Monad.Catch
    ( MonadCatch, SomeException(..), catch, displayException, throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (LoggingT, MonadLogger, logErrorN)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, MonadResource)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT)
import Data.Pool (Pool)
import Database.Persist.Quasi
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text)
import qualified Data.Text as T

type BaseM = ResourceT (LoggingT IO)
type SqlTx = ReaderT SqlBackend BaseM
type SqlM = ReaderT (Pool SqlBackend) BaseM

type SqlRecord t = PersistRecordBackend t SqlBackend

runSql :: SqlTx a -> SqlM a
runSql = ReaderT . runSqlPool

withLoggedExceptions :: (MonadCatch m, MonadLogger m) => String -> m r -> m r
withLoggedExceptions msg act = act `catch`  \(SomeException e) -> do
    logErrorN . T.pack $ msg ++ displayException e
    throwM e

showSqlKey :: ToBackendKey SqlBackend record => Key record -> Text
showSqlKey = T.pack . show . fromSqlKey

whenNotExists
    :: SqlRecord record
    => [Filter record]
    -> ConduitT i a SqlTx ()
    -> ConduitT i a SqlTx ()
whenNotExists filters act = lift (selectFirst filters []) >>= \case
    Just _ -> return ()
    Nothing -> act

getUniq :: SqlRecord record => record -> SqlTx (Key record)
getUniq record = do
    result <- getBy =<< onlyUnique record
    case result of
        Nothing -> insert record
        Just (Entity k _) -> return k

insertUniq :: (Eq record, Show record, SqlRecord record) => record -> SqlTx ()
insertUniq record = do
    result <- insertBy record
    case result of
        Left (Entity _ r) | record /= r -> logErrorN . T.pack $ mconcat
            ["Unique insert failed:\nFound: ", show r, "\nNew: ", show record]
        _ -> return ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings "src/schema")
