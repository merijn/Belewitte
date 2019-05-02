{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE ViewPatterns #-}
module Schema.Utils
    ( Int64
    , MigrationAction
    , (.=)
    , mkMigration
    , mkMigrationLookup
    ) where

import Control.Monad.Reader (ReaderT)
import Data.Int (Int64)
import qualified Data.Map as M
import Database.Persist.Sql (EntityDef, Migration, SqlBackend, migrate)
import Database.Persist.TH (embedEntityDefs)

type MigrationAction = ReaderT SqlBackend IO [EntityDef]

(.=) :: Functor f => a -> b -> f c -> (a, f b)
(.=) i schema act = (i, schema <$ act)

mkMigration :: [[EntityDef]] -> Migration
mkMigration ents = mapM_ (migrate embeddedEnts) embeddedEnts
  where
    embeddedEnts = embedEntityDefs . concat $ ents

mkMigrationLookup
    :: [EntityDef] -> [(Int64, MigrationAction)] -> Int64 -> MigrationAction
mkMigrationLookup latestSchema (M.fromList -> migrationMap) = \i ->
    case M.lookupLE i migrationMap of
        Nothing -> return latestSchema
        Just (_, m) -> m
