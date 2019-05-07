{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Migration (checkMigration) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, withRunInIO)
import Control.Monad.Logger (MonadLogger)
import qualified Control.Monad.Logger as Log
import Control.Monad.Reader (ReaderT)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Typeable (Typeable)
import Database.Persist.Sql
    (Migration, PersistField, Single(Single), SqlBackend)
import qualified Database.Persist.Sql as Sql
import Database.Persist.Sqlite (RawSqlite)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (Handle, IOMode(WriteMode), hClose, stderr, withFile)

import Exceptions
import Schema

type SqlM m = ReaderT (RawSqlite SqlBackend) m

setPragma :: (MonadIO m, Show v) => Text -> v -> SqlM m ()
setPragma pragma val = Sql.liftPersist $ Sql.rawExecute query []
  where
    query = "PRAGMA " <> pragma <> " = " <> T.pack (show val)

withSilencedHandle :: MonadUnliftIO m => Handle -> m a -> m a
withSilencedHandle hnd action = withRunInIO $ \runInIO ->
    withFile "/dev/null" WriteMode $ \devNull ->
        bracket (alloc devNull) cleanup $ \_ -> runInIO action
  where
    alloc devNull = do
        hDuplicate hnd <* hDuplicateTo devNull hnd

    cleanup oldStdErr = do
        hDuplicateTo oldStdErr hnd
        hClose oldStdErr

silencedUnsafeMigration :: Migration -> ReaderT SqlBackend IO ()
silencedUnsafeMigration m = withSilencedHandle stderr $ do
    Sql.runMigrationUnsafe m

querySingleValue
    :: (MonadIO m, MonadLogger m, MonadThrow m, PersistField a, Show a)
    => Text
    -> [PersistValue]
    -> SqlM m a
querySingleValue query args = do
    result <- Sql.liftPersist $ Sql.rawSql query args
    case result of
        [Single v] -> return v
        v -> logThrowM . ExpectedSingleValue query $ show v

checkSchema
    :: (MonadIO m, MonadLogger m, MonadThrow m) => Migration -> SqlM m ()
checkSchema schema = do
    noChanges <- null <$> Sql.liftPersist (Sql.getMigration schema)
    unless noChanges $ logThrowM WrongSchema

validateSchema
    :: (MonadCatch m, MonadIO m, MonadLogger m, MonadThrow m)
    => Bool -> Int64 -> SqlM m Bool
validateSchema _ v
    | v > schemaVersion = logThrowM $ TooNew v
    | v == schemaVersion = False <$ checkSchema currentSchema

validateSchema migrateSchema version
    | not migrateSchema = logThrowM $ MigrationNeeded version
    | otherwise = True <$ do
        Log.logInfoN $ "Migrating schema."
        forM_ [version..schemaVersion - 1] $ \n -> do
            Log.logInfoN $ mconcat
                [ "Migrating file from schema version ", T.pack (show n)
                , " to version " , T.pack (show (n+1)), "." ]

            Sql.rawExecute "BEGIN TRANSACTION" []
            setPragma "defer_foreign_keys" (1 :: Int64)
            migration <- Sql.liftPersist $ do
                migration <- schemaUpdateForVersion n
                migration <$ silencedUnsafeMigration migration

            checkSchema migration `catch` migrationFailed (n+1)
            Sql.rawExecute "COMMIT TRANSACTION" []
            setPragma "user_version" $ (n + 1 :: Int64)

            Log.logInfoN $ mconcat
                [ "Succesfully migrated to version ", T.pack (show (n+1)), "!"]

        Log.logInfoN $ "Migration complete!"
  where
    migrationFailed
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => Int64 -> SchemaWrong -> SqlM m a
    migrationFailed n WrongSchema = do
        Sql.rawExecute "ROLLBACK TRANSACTION" []
        Log.logErrorN $ mconcat
            [ "Migration failed while migrating to version ", T.pack (show n)
            , ".\nRolling back to version ", T.pack (show (n-1))
            ]
        logThrowM AbortMigration

checkMigration
    :: (MonadCatch m, MonadIO m, MonadLogger m, MonadThrow m)
    => Bool -> SqlM m Bool
checkMigration migrateSchema = do
    tableCount <- querySingleValue "SELECT COUNT(*) FROM sqlite_master" []
    case tableCount :: Int64 of
        0 -> do
            Sql.liftPersist $ Sql.runMigrationSilent currentSchema
            checkSchema currentSchema
            False <$ setPragma "user_version" schemaVersion
        _ -> do
            version <- querySingleValue "PRAGMA user_version" []
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
