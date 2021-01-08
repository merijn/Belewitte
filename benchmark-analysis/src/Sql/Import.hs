{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Import
    ( MonadImport(importSql, importSqlTransaction)
    , Import
    , Importable(..)
    , runImport
    , runRegionConduit
    , selectKeysImport
    , selectSourceImport
    , importEntity
    ) where

import Control.Monad ((>=>), join)
import Control.Monad.Catch (MonadCatch, MonadMask, throwM, try)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Acquire (Acquire)
import Data.Conduit (ConduitT, toProducer)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Database.Persist.Class (keyFromRecordM)
import qualified Database.Persist.Sqlite as Sqlite

import Exceptions (MissingPrimaryKey(..), MissingUniqEntity(..), logThrowM)
import Migration (checkMigration)
import Schema.Import (ImportType(..), Importable(..), UpdateField(..))
import Schema.Utils (getTypeName)
import Sql.Core
import qualified Sql.Transaction as SqlTrans

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

instance MonadImport m => MonadImport (ConduitT i o m) where
    importConnection = lift importConnection
    importSql query = lift $ importSql query
    importSqlTransaction t = lift $ importSqlTransaction t

instance MonadImport m => MonadImport (Region m) where
    importConnection = lift importConnection
    importSql query = lift $ importSql query
    importSqlTransaction t = lift $ importSqlTransaction t

instance MonadImport m => MonadImport (Transaction m) where
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

importEntity
    :: (Importable rec, MonadCatch m, MonadImport m, MonadSql m)
    => Entity rec -> Transaction m ()
importEntity ent = do
    val <- entityVal <$> updateEntity False ent
    case importType val of
        UniqueImport -> () <$ SqlTrans.insertUniq val
        ExplicitUniqueImport{} -> () <$ SqlTrans.insertUniq val

        PrimaryImport -> do
            key <- case keyFromRecordM <*> pure val of
                Just key -> return key
                Nothing -> logThrowM . MissingPrimaryKey $ getTypeName ent

            () <$ SqlTrans.insertKey key val

updateEntity
    :: (Importable rec, MonadCatch m, MonadImport m, MonadSql m)
    => Bool -> Entity rec -> Transaction m (Entity rec)
updateEntity allowMissing = foldr (>=>) return $
    map updateFromField updateFields
  where
    updateFromField
        :: (MonadCatch m, MonadImport m, MonadSql m, SqlRecord rec)
        => UpdateField rec -> Entity rec -> Transaction m (Entity rec)
    updateFromField updateField val = case updateField of
        ForeignKeyField field ->
            fieldLens field (translateImportKey False) val

        ForeignKeyFieldAllowMissing field ->
            fieldLens field (translateImportKey (True && allowMissing)) val

uniqueLookup
    :: forall rec m .
       ( AtLeastOneUniqueKey rec
       , Importable rec
       , MonadLogger m
       , MonadSql m
       , MonadThrow m
       , Show (Unique rec)
       )
    => (Unique rec -> Bool) -> rec -> Transaction m (Key rec)
uniqueLookup f val = case NonEmpty.filter f (Sqlite.requireUniquesP val) of
    (uniqFromVal : _) -> SqlTrans.getJustKeyBy uniqFromVal
    [] -> logThrowM . MissingPrimaryKey $ getTypeName (Proxy :: Proxy rec)

translateImportKey
    :: forall rec m . (Importable rec, MonadCatch m, MonadImport m, MonadSql m)
    => Bool -> Key rec -> Transaction m (Key rec)
translateImportKey allowMissing k = do
    originalVal <- importSqlTransaction $ SqlTrans.getJustEntity k
    updatedVal <- entityVal <$> updateEntity True originalVal
    result <- try $ case importType updatedVal of
        UniqueImport -> uniqueLookup (const True) updatedVal
        ExplicitUniqueImport f -> uniqueLookup f updatedVal
        PrimaryImport -> case keyFromRecordM <*> pure updatedVal of
            Just key -> key <$ SqlTrans.getJust key
            Nothing -> logThrowM . MissingPrimaryKey $
                getTypeName (Proxy :: Proxy rec)

    case result of
        Right v -> return v
        Left MissingUniqEntity{} | allowMissing -> return k
        Left exc -> throwM exc
