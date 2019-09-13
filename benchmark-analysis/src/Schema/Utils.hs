{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE ViewPatterns #-}
module Schema.Utils
    ( MonadSql
    , EntityDef
    , Int64
    , (.=)
    , (.>)
    , executeSql
    , mkMigration
    , mkMigrationLookup
    ) where

import Data.Int (Int64)
import qualified Data.Map as M
import Database.Persist.Sql (EntityDef, Migration, migrate)
import Database.Persist.TH (embedEntityDefs)

import Sql.Core (MonadSql, executeSql)

(.=) :: Applicative f => a -> b -> (a, (b, f ()))
(.=) i schema = (i, (schema, pure ()))

(.>) :: Functor f => a -> b -> f c -> (a, (b, f c))
(.>) i schema act = (i, (schema, act))

mkMigration :: [[EntityDef]] -> Migration
mkMigration ents = mapM_ (migrate embeddedEnts) embeddedEnts
  where
    embeddedEnts = embedEntityDefs . concat $ ents

mkMigrationLookup
    :: MonadSql m
    => [(Int64, ([EntityDef], m ()))] -> Int64 -> m [EntityDef]
mkMigrationLookup (M.fromList -> migrationMap) = \i ->
    case M.lookupLE i migrationMap of
        Nothing -> return []
        Just (key, (schema, migration))
            | key == i -> schema <$ migration
            | otherwise -> return schema
