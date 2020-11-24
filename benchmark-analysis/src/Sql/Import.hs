{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Sql.Import
    ( Import
    , runImport
    , importSql
    , importSqlTransaction
    ) where

import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Resource (MonadResource)
import Data.Text (Text)

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
