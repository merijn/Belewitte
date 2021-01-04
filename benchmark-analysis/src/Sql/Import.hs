{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Import
    ( MonadImport(importSql, importSqlTransaction)
    , Import
    , runImport
    , runRegionConduit
    , selectKeysImport
    , selectSourceImport
    , translateImportKey
    ) where

import Control.Monad (join)
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Acquire (Acquire)
import Data.Conduit (ConduitT, toProducer)
import Data.Text (Text)
import qualified Database.Persist.Sqlite as Sqlite

import Migration (checkMigration)
import Sql.Core
import qualified Sql

type ImportConstraint m = (MonadLogger m, MonadResource m, MonadThrow m)
type SqlConstraint m = (MonadSql m, MonadLogger m, MonadThrow m)

class ImportConstraint m => MonadImport m where
    importConnection :: m (Acquire (RawSqlite SqlBackend))
    default importConnection
        :: (MonadImport n, MonadTrans t, m ~ t n)
        => m (Acquire (RawSqlite SqlBackend))
    importConnection = lift importConnection

    importSql :: (forall n . SqlConstraint n => n r) -> m r
    importSqlTransaction
        :: (forall n . SqlConstraint n => Transaction n r)
        -> m r

newtype Import m r = Import { unImport :: SqlT m r }
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadLogger
  , MonadLoggerIO, MonadMask, MonadResource, MonadThrow)

instance MonadTrans Import where
    lift = Import . lift

instance MonadSql m => MonadSql (Import m) where
    getConnFromPool = lift getConnFromPool
    getConnWithoutForeignKeysFromPool = lift getConnWithoutForeignKeysFromPool
    getConnWithoutTransaction = lift getConnWithoutTransaction

instance ImportConstraint m => MonadImport (Import m) where
    importConnection = Import getConnFromPool
    importSql query = Import query
    importSqlTransaction t = Import $ runReadOnlyTransaction t

instance MonadImport m => MonadImport (Region m) where
    importConnection = lift importConnection
    importSql query = lift $ importSql query
    importSqlTransaction t = lift $ importSqlTransaction t

instance MonadImport m => MonadImport (ConduitT i o m) where
    importConnection = lift importConnection
    importSql query = lift $ importSql query
    importSqlTransaction t = lift $ importSqlTransaction t

selectKeysImport
    :: (MonadImport m, SqlRecord record)
    => [Filter record]
    -> [SelectOpt record]
    -> ConduitT a (Key record) (Region m) ()
selectKeysImport filts order = do
    acquireConn <- importConnection
    (key, source) <- allocRegion $ do
        conn <- acquireConn >>= readOnlyConnection
        join $ runReaderT (Sqlite.selectKeysRes filts order) conn

    toProducer source <* release key

selectSourceImport
    :: (MonadImport m, SqlRecord record)
    => [Filter record]
    -> [SelectOpt record]
    -> ConduitT a (Entity record) (Region m) ()
selectSourceImport filts order = do
    acquireConn <- importConnection
    (key, source) <- allocRegion $ do
        conn <- acquireConn >>= readOnlyConnection
        join $ runReaderT (Sqlite.selectSourceRes filts order) conn

    toProducer source <* release key

runImport
    :: ( MonadLogger m
       , MonadMask m
       , MonadResource m
       , MonadThrow m
       , MonadUnliftIO m
       )
    => Text -> Import m r -> m r
runImport database act = runSqlT database $
    checkMigration False >> unImport act

translateImportKey
    :: ( MonadImport m
       , MonadSql m
       , MonadThrow m
       , Show (Unique rec)
       , Sqlite.OnlyOneUniqueKey rec
       , SqlRecord rec
       )
    => Key rec -> m (Key rec)
translateImportKey k = do
    originalVal <- importSql $ Sql.getJust k >>= Sql.onlyUnique
    Sql.getJustKeyBy originalVal
