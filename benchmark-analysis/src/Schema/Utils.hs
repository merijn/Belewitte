{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Schema.Utils
    ( MonadLogger
    , MonadSql
    , MonadThrow
    , Transaction
    , Entity
    , EntityDef
    , ForeignDef
    , HaskellName(..)
    , Int64
    , (.=)
    , (.>)
    , Sql.executeSql
    , Sql.executeSqlSingleValue
    , Sql.executeSqlSingleValueMaybe
    , createTableFromSchema
    , mkForeignRef
    , addForeignRef
    , mkEntities
    , mkMigration
    , mkMigrationLookup
    , getTypeName
    ) where

import Control.Monad (void)
import Data.Int (Int64)
import qualified Data.Map as M
import Data.Text (Text)
import Database.Persist.Sql (Entity, Migration, entityDef, migrate)
import Database.Persist.TH (embedEntityDefs)
import Database.Persist.Types
    (DBName(..), EntityDef(..), ForeignDef(..), HaskellName(..), noCascade)

import Schema.Utils.TH (mkEntities)
import Sql.Core (MonadLogger, MonadSql, MonadThrow, SqlRecord, Transaction)
import qualified Sql.Core as Sql

(.=) :: Applicative f => a -> b -> (a, (b, f ()))
(.=) i schema = (i, (schema, pure ()))

(.>) :: Functor f => a -> b -> f c -> (a, (b, f c))
(.>) i schema act = (i, (schema, act))

mkMigration :: [[EntityDef]] -> Migration
mkMigration ents = mapM_ (migrate embeddedEnts) embeddedEnts
  where
    embeddedEnts = embedEntityDefs . concat $ ents

createTableFromSchema :: MonadSql m => [EntityDef] -> Transaction m ()
createTableFromSchema = void . Sql.runMigrationQuiet . mkMigration . pure

mkForeignRef :: Text -> [(Text, Text)] -> ForeignDef
mkForeignRef foreignTable refs = ForeignDef
    { foreignRefTableHaskell = HaskellName foreignTable
    , foreignRefTableDBName = DBName foreignTable
    , foreignConstraintNameHaskell = HaskellName $ "Foreign" <> foreignTable
    , foreignConstraintNameDBName = DBName $ "Foreign" <> foreignTable
    , foreignFields = map wrapNames refs
    , foreignFieldCascade = noCascade
    , foreignToPrimary = False
    , foreignAttrs = []
    , foreignNullable = False
    }
  where
    wrapNames :: (Text, Text) -> ((HaskellName, DBName), (HaskellName, DBName))
    wrapNames (src, dst) =
        ((HaskellName src, DBName src), (HaskellName dst, DBName dst))

addForeignRef :: Text -> ForeignDef -> [EntityDef] -> [EntityDef]
addForeignRef _ _ [] = []
addForeignRef name fk (ent:ents)
    | entityHaskell ent /= HaskellName name = ent : addForeignRef name fk ents
    | otherwise = ent { entityForeigns = fk : entityForeigns ent } : ents

mkMigrationLookup
    :: MonadSql m
    => [(Int64, ([EntityDef], Transaction m ()))]
    -> Int64
    -> Transaction m [EntityDef]
mkMigrationLookup (M.fromList -> migrationMap) = \i ->
    case M.lookupLE i migrationMap of
        Nothing -> return []
        Just (key, (schema, migration))
            | key == i -> schema <$ migration
            | otherwise -> return schema

getTypeName :: SqlRecord rec => proxy rec -> Text
getTypeName = unHaskellName . entityHaskell . entityDef
