{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Migration (checkMigration) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Catch (MonadMask, MonadThrow, catch, onError)
import Control.Monad.Logger (MonadLogger, logWarnN)
import qualified Control.Monad.Logger as Log
import qualified Data.Conduit.Combinators as C
import Data.Monoid (Any(..))
import Data.Int (Int64)
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Exceptions
import Pretty ((<+>))
import qualified Pretty
import Schema
import Sql.Core (Migration, MonadSql, Transaction)
import qualified Sql.Core as Sql

data MigrationSafety = MigrateSafe | MigrateUnsafe

showText :: Show a => a -> Text
showText = T.pack . show

checkForeignKeys
    :: (MonadLogger m, MonadSql m, MonadThrow m) => Transaction m ()
checkForeignKeys = do
    result <- Sql.sinkQuery query [] $ C.foldMapM logViolation
    when (getAny result) $ logThrowM ForeignKeyViolation
  where
    logViolation l = Any True <$ logWarnN errorMsg
      where
        errorMsg = case l of
            [ PersistInt64 rowid , PersistText table , PersistText column ] ->
                mconcat [ "Foreign key violation for table \"", table
                        , "\" rowid #" , showText rowid, " column \"", column
                        , "\"!" ]
            _ -> mconcat
                [ "Unexpected result from foreign key check:\n", showText l ]

    query = [i|
SELECT origin.rowid, origin."table", group_concat(foreignkeys."from")
FROM pragma_foreign_key_check() AS origin
INNER JOIN pragma_foreign_key_list(origin."table") AS foreignkeys
ON origin.fkid = foreignkeys.id AND origin.parent = foreignkeys."table"
GROUP BY origin.rowid
|]

checkSchema
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Migration -> Transaction m ()
checkSchema schema = do
    noChanges <- null <$> Sql.getMigration schema
    unless noChanges $ logThrowM WrongSchema

validateSchema
    :: (MonadLogger m, MonadMask m, MonadSql m)
    => Bool -> Int64 -> m Bool
validateSchema _ v
    | v > schemaVersion = logThrowM $ TooNew v
    | v == schemaVersion = Sql.runTransaction $
                                False <$ checkSchema currentSchema

validateSchema migrateSchema version
    | not migrateSchema = logThrowM $ MigrationNeeded version
    | otherwise = True <$ migrateFromTo MigrateSafe version schemaVersion

migrateFromTo
    :: (MonadLogger m, MonadMask m, MonadSql m)
    => MigrationSafety -> Int64 -> Int64 -> m ()
migrateFromTo safety originalVersion finalVersion = do
    case safety of
        MigrateUnsafe -> return ()
        MigrateSafe -> Sql.runTransaction checkForeignKeys

    Log.logInfoN $ "Migrating schema."
    forM_ [originalVersion..finalVersion - 1] $ \n -> do
        Log.logInfoN $ mconcat
            [ "Migrating file from schema version ", showText n
            , " to version " , showText (n+1), "." ]

        reportMigrationFailure n . Sql.runTransactionWithoutForeignKeys $ do
            migration <- updateSchemaToVersion (n+1)
            Sql.runMigrationUnsafeQuiet migration
            updateIndicesToVersion (n+1)

            checkSchema migration `catch` migrationFailed

            checkForeignKeys

            Sql.setPragma "user_version" (n + 1 :: Int64)

        Log.logInfoN $ mconcat
            [ "Succesfully migrated to version ", showText (n+1), "!"]

    Log.logInfoN $ "Migration complete!"
  where
    reportMigrationFailure
        :: (MonadLogger m, MonadMask m) => Int64 -> m a -> m a
    reportMigrationFailure n act = onError act $ do
        Log.logErrorN $ mconcat
            [ "Migration failed while migrating to version ", showText (n+1)
            , ".\nRolling back to version ", showText n
            ]

    migrationFailed :: (MonadLogger m, MonadThrow m) => SchemaWrong -> m a
    migrationFailed WrongSchema = logThrowM AbortMigration

checkMigration :: (MonadLogger m, MonadMask m, MonadSql m) => Bool -> m Bool
checkMigration migrateSchema = do
    tableCount <- Sql.querySingleValue "SELECT COUNT(*) FROM sqlite_master" []

    case tableCount :: Int64 of
        0 -> Sql.runTransaction $ do
            Sql.runMigrationQuiet currentSchema
            checkSchema currentSchema
            updateIndicesToVersion schemaVersion
            False <$ Sql.setPragma "user_version" schemaVersion
        _ -> do
            version <- Sql.querySingleValue "PRAGMA user_version" []
            validateSchema migrateSchema version

data SchemaTooNew = TooNew Int64
    deriving (Show, Typeable)

instance Pretty SchemaTooNew where
    pretty (TooNew n) = Pretty.vsep
        [ "Schema too new."
        , "Expected schema version:" <+> pretty schemaVersion
        , "Found schema version:" <+> pretty n
        ]

instance Exception SchemaTooNew where
    toException = toSchemaException
    fromException = fromSchemaException
    displayException = show . pretty

data MigrationNeeded = MigrationNeeded Int64
    deriving (Show, Typeable)

instance Pretty MigrationNeeded where
    pretty (MigrationNeeded v) = Pretty.vsep
        [ "Schema migration needed."
        , "Found schema version:" <+> pretty v
        , "Current schema version:" <+> pretty schemaVersion
        , "Use --migrate to run an automatic migration."
        , ""
        , "CAUTION: Make sure to back up the data before migrating!"
        ]

instance Exception MigrationNeeded where
    toException = toSchemaException
    fromException = fromSchemaException
    displayException = show . pretty
