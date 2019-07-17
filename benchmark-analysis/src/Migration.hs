{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Migration (checkMigration) where

import Control.Monad (forM_, unless)
import Control.Monad.Catch (MonadMask, MonadThrow, bracket, catch, onError)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Logger (MonadLogger)
import qualified Control.Monad.Logger as Log
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Typeable (Typeable)
import Database.Persist.Sql (Migration, PersistField, Single(Single))
import qualified Database.Persist.Sql as Sql
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (Handle, IOMode(WriteMode), hClose, stderr, withFile)

import Exceptions
import Schema

showText :: Show a => a -> Text
showText = T.pack . show

setPragma :: (MonadMigrate m, Show v) => Text -> v -> m ()
setPragma pragma val = executeMigrationSql query
  where
    query = "PRAGMA " <> pragma <> " = " <> showText val

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

silencedUnsafeMigration :: MonadMigrate m => Migration -> m ()
silencedUnsafeMigration m = liftMigration . withSilencedHandle stderr $ do
    Sql.runMigrationUnsafe m

querySingleValue
    :: (MonadMigrate m, MonadLogger m, MonadThrow m, PersistField a, Show a)
    => Text
    -> [PersistValue]
    -> m a
querySingleValue query args = do
    result <- liftMigration $ Sql.rawSql query args
    case result of
        [Single v] -> return v
        v -> logThrowM . ExpectedSingleValue query $ show v

checkSchema
    :: (MonadMigrate m, MonadLogger m, MonadThrow m) => Migration -> m ()
checkSchema schema = do
    noChanges <- null <$> liftMigration (Sql.getMigration schema)
    unless noChanges $ logThrowM WrongSchema

validateSchema
    :: (MonadMigrate m, MonadLogger m, MonadMask m)
    => Bool -> Int64 -> m Bool
validateSchema _ v
    | v > schemaVersion = logThrowM $ TooNew v
    | v == schemaVersion = False <$ checkSchema currentSchema

validateSchema migrateSchema version
    | not migrateSchema = logThrowM $ MigrationNeeded version
    | otherwise = True <$ do
        Log.logInfoN $ "Migrating schema."
        forM_ [version..schemaVersion - 1] $ \n -> do
            Log.logInfoN $ mconcat
                [ "Migrating file from schema version ", showText n
                , " to version " , showText (n+1), "." ]

            reportMigrationFailure n $ do
                executeMigrationSql "BEGIN TRANSACTION"
                setPragma "defer_foreign_keys" (1 :: Int64)

                migration <- schemaUpdateForVersion n
                silencedUnsafeMigration migration

                checkSchema migration `catch` migrationFailed

                executeMigrationSql "COMMIT TRANSACTION"
                setPragma "user_version" $ (n + 1 :: Int64)

            Log.logInfoN $ mconcat
                [ "Succesfully migrated to version ", showText (n+1), "!"]

        Log.logInfoN $ "Migration complete!"
  where
    reportMigrationFailure
        :: (MonadMigrate m, MonadLogger m, MonadMask m) => Int64 -> m a -> m a
    reportMigrationFailure n act = onError act $ do
        executeMigrationSql "ROLLBACK TRANSACTION"
        Log.logErrorN $ mconcat
            [ "Migration failed while migrating to version ", showText (n+1)
            , ".\nRolling back to version ", showText n
            ]

    migrationFailed
        :: (MonadMigrate m, MonadLogger m, MonadThrow m) => SchemaWrong -> m a
    migrationFailed WrongSchema = logThrowM AbortMigration

checkMigration
    :: (MonadMigrate m, MonadLogger m, MonadMask m)
    => Bool -> m Bool
checkMigration migrateSchema = do
    tableCount <- querySingleValue "SELECT COUNT(*) FROM sqlite_master" []
    case tableCount :: Int64 of
        0 -> do
            liftMigration $ Sql.runMigrationSilent currentSchema
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
