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
    , mkEntitiesWith
    , mkMigration
    , mkMigrationLookup
    , mkSchema
    , getTypeName
    ) where

import Control.Exception (ErrorCall(..), fromException)
import Control.Monad (void)
import qualified Control.Monad.Logger as Log
import Control.Monad.Catch (throwM)
import Data.Int (Int64)
import Data.List (stripPrefix)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.EntityDef.Internal (EntityDef(..))
import Database.Persist.Quasi (PersistSettings)
import qualified Database.Persist.Quasi as Quasi
import Database.Persist.Quasi.Internal (UnboundEntityDef)
import Database.Persist.Sql (Entity, Migration, entityDef)
import Database.Persist.TH (migrateModels)
import qualified Database.Persist.TH as TH
import Database.Persist.Types
    ( ConstraintNameDB(..)
    , ConstraintNameHS(..)
    , EntityNameDB(..)
    , EntityNameHS(..)
    , FieldNameDB(..)
    , FieldNameHS(..)
    , ForeignDef(..)
    , noCascade
    )
import Language.Haskell.TH (Dec, Q)
import Language.Haskell.TH.Quote (QuasiQuoter)

import Sql.Core (MonadLogger, MonadSql, MonadThrow, SqlRecord, Transaction)
import qualified Sql.Core as Sql

(.=) :: Applicative f => a -> b -> (a, (b, f ()))
(.=) i schema = (i, (schema, pure ()))

(.>) :: Functor f => a -> b -> f c -> (a, (b, f c))
(.>) i schema act = (i, (schema, act))

mkSchema :: QuasiQuoter
mkSchema = TH.persistWith schemaSettings
  where
    constraintName :: EntityNameHS -> ConstraintNameHS -> Text
    constraintName (EntityNameHS ent) (ConstraintNameHS constraint) =
      case T.stripPrefix "foreign" constraint of
          Nothing -> ent <> constraint
          Just remainder -> "Foreign" <> remainder

    schemaSettings :: PersistSettings
    schemaSettings = Quasi.setPsToFKName constraintName Quasi.upperCaseSettings

mkEntities :: String -> [UnboundEntityDef] -> Q [Dec]
mkEntities name = TH.share
    [TH.mkPersist TH.sqlSettings, TH.mkEntityDefList name]

mkEntitiesWith :: String -> [[EntityDef]] -> [UnboundEntityDef] -> Q [Dec]
mkEntitiesWith name entLists = TH.share
    [TH.mkPersistWith TH.sqlSettings ents, TH.mkEntityDefList name]
  where
    ents :: [EntityDef]
    ents = mconcat entLists

mkMigration
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => [[EntityDef]] -> Transaction m Migration
mkMigration ents = do
    result <- Sql.getMigration resolvedEnts
    case result of
        Right sql -> do
            Log.logInfoN $ T.unlines sql
            return resolvedEnts

        Left exc -> do
            case fromException exc of
                Just (ErrorCall e) -> reportError e
                _ -> Log.logErrorN "Unknown error while generating migration"

            throwM exc
  where
    resolvedEnts :: Migration
    resolvedEnts = migrateModels . concat $ ents

    reportError :: MonadLogger m => String -> Transaction m ()
    reportError msg
        | Just _name <- stripPrefix "Table not found: " msg
        = Log.logErrorN . T.unlines $
            [ T.pack msg, "Found tables:" ]
            ++ map (unEntityNameHS . entityHaskell) (concat ents)

        | otherwise = Log.logErrorN "Unknown error while generating migration"

createTableFromSchema
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => [[EntityDef]] -> Transaction m ()
createTableFromSchema defs = do
    schema <- mkMigration defs
    void $ Sql.runMigrationQuiet schema

mkForeignRef :: Text -> [(Text, Text)] -> ForeignDef
mkForeignRef foreignTable refs = ForeignDef
    { foreignRefTableHaskell = EntityNameHS foreignTable
    , foreignRefTableDBName = EntityNameDB foreignTable
    , foreignConstraintNameHaskell =
        ConstraintNameHS $ "Foreign" <> foreignTable
    , foreignConstraintNameDBName =
        ConstraintNameDB $ "Foreign" <> foreignTable
    , foreignFields = map wrapNames refs
    , foreignFieldCascade = noCascade
    , foreignToPrimary = False
    , foreignAttrs = []
    , foreignNullable = False
    }
  where
    wrapNames
        :: (Text, Text)
        -> ((FieldNameHS, FieldNameDB), (FieldNameHS, FieldNameDB))
    wrapNames (src, dst) =
      ((FieldNameHS src, FieldNameDB src), (FieldNameHS dst, FieldNameDB dst))

addForeignRef :: Text -> ForeignDef -> [EntityDef] -> [EntityDef]
addForeignRef _ _ [] = []
addForeignRef name fk (ent:ents)
    | entityHaskell ent /= EntityNameHS name = ent : addForeignRef name fk ents
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
getTypeName = unEntityNameHS . entityHaskell . entityDef
