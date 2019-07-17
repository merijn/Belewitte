{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE ViewPatterns #-}
module Schema.Utils
    ( EntityDef
    , Int64
    , MonadMigrate
    , (.=)
    , executeMigrationSql
    , liftMigration
    , mkMigration
    , mkMigrationLookup
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, withReaderT)
import Data.Int (Int64)
import qualified Data.Map as M
import Data.Text (Text)
import Database.Persist.Class (projectBackend)
import Database.Persist.Sql (EntityDef, Migration, SqlBackend)
import qualified Database.Persist.Sql as Sql
import Database.Persist.Sqlite (RawSqlite)
import Database.Persist.TH (embedEntityDefs)

type MonadMigrate m = (MonadIO m, MonadReader (RawSqlite SqlBackend) m)

(.=) :: Functor f => a -> b -> f c -> (a, f b)
(.=) i schema act = (i, schema <$ act)

executeMigrationSql :: MonadMigrate m => Text -> m ()
executeMigrationSql query = liftMigration $ Sql.rawExecute query []

liftMigration :: MonadMigrate m => ReaderT SqlBackend IO r -> m r
liftMigration = Sql.liftPersist . withReaderT projectBackend

mkMigration :: [[EntityDef]] -> Migration
mkMigration ents = mapM_ (Sql.migrate embeddedEnts) embeddedEnts
  where
    embeddedEnts = embedEntityDefs . concat $ ents

mkMigrationLookup
    :: MonadMigrate m
    => [EntityDef] -> [(Int64, m [EntityDef])] -> Int64 -> m [EntityDef]
mkMigrationLookup latestSchema (M.fromList -> migrationMap) = \i ->
    case M.lookup i migrationMap of
        Nothing -> return latestSchema
        Just m -> m
