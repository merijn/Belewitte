{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Import
    ( Import
    , runImport
    , runRegionConduit
    , importSql
    , importSqlTransaction
    , selectKeysImport
    , selectSourceImport
    ) where

import Control.Monad (join)
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Conduit (ConduitT, toProducer)
import Data.Text (Text)
import qualified Database.Persist.Sqlite as Sqlite

import Migration (checkMigration)
import Sql.Core

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

selectKeysImport
    :: (MonadResource m, SqlRecord record)
    => [Filter record]
    -> [SelectOpt record]
    -> ConduitT a (Key record) (Region (Import m)) ()
selectKeysImport filts order = do
    acquireConn <- lift . lift $ Import getConnFromPool
    (key, source) <- allocRegion $ do
        conn <- acquireConn >>= readOnlyConnection
        join $ runReaderT (Sqlite.selectKeysRes filts order) conn

    toProducer source <* release key

selectSourceImport
    :: (MonadResource m, SqlRecord record)
    => [Filter record]
    -> [SelectOpt record]
    -> ConduitT a (Entity record) (Region (Import m)) ()
selectSourceImport filts order = do
    acquireConn <- lift . lift $ Import getConnFromPool
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

importSql
    :: (MonadLogger m, MonadResource m, MonadThrow m)
    => (forall n . (MonadSql n, MonadLogger n, MonadThrow n) => n r)
    -> Import m r
importSql query = Import query

importSqlTransaction
    :: (MonadLogger m, MonadResource m, MonadThrow m)
    => (forall n . (MonadSql n, MonadLogger n, MonadThrow n) => Transaction n r)
    -> Import m r
importSqlTransaction transaction = Import $ runReadOnlyTransaction transaction
