{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Schema
    ( ByteString
    , Hash(..)
    , ImplType(..)
    , Model
    , MonadMigrate
    , executeMigrationSql
    , liftMigration
    , PersistValue(..)
    , toPersistValue
    , Text
    , module Schema.Algorithm
    , module Schema.Graph
    , module Schema.Implementation
    , module Schema.Model
    , module Schema.Platform
    , module Schema.Properties
    , module Schema.Timers
    , module Schema.Variant
    , bestNonSwitchingImplId
    , predictedImplId
    , optimalImplId
    , getAlgoName
    , getImplName
    , schemaVersion
    , currentSchema
    , schemaUpdateForVersion
    ) where

import Control.Monad.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Persist.Sql
    (EntityDef, Migration, PersistValue(..), SqlBackend, toPersistValue)
import Database.Persist.Sqlite (RawSqlite)

import Model (Model)
import Schema.Utils
    (MonadMigrate, executeMigrationSql, liftMigration, mkMigration)
import Types

import Schema.Algorithm hiding (migrations, schema)
import qualified Schema.Algorithm as Algorithm
import Schema.Graph hiding (migrations, schema)
import qualified Schema.Graph as Graph
import Schema.Implementation hiding (migrations, schema)
import qualified Schema.Implementation as Implementation
import Schema.Model hiding (migrations, schema)
import qualified Schema.Model as Model
import Schema.Platform hiding (migrations, schema)
import qualified Schema.Platform as Platform
import Schema.Properties hiding (migrations, schema)
import qualified Schema.Properties as Properties
import Schema.Timers hiding (migrations, schema)
import qualified Schema.Timers as Timers
import Schema.Variant hiding (migrations, schema)
import qualified Schema.Variant as Variant

bestNonSwitchingImplId :: Integral n => n
bestNonSwitchingImplId = -1

predictedImplId :: Integral n => n
predictedImplId = -2

optimalImplId :: Integral n => n
optimalImplId = -3

getAlgoName :: Algorithm -> Text
getAlgoName Algorithm{algorithmName,algorithmPrettyName} =
  fromMaybe algorithmName algorithmPrettyName

getImplName :: Implementation -> Text
getImplName Implementation{implementationName,implementationPrettyName} =
  fromMaybe implementationName implementationPrettyName

migrations :: MonadMigrate m => [([EntityDef], Int64 -> m [EntityDef])]
migrations =
    [ (Platform.schema, Platform.migrations)
    , (Graph.schema, Graph.migrations)
    , (Algorithm.schema, Algorithm.migrations)
    , (Implementation.schema, Implementation.migrations)
    , (Variant.schema, Variant.migrations)
    , (Properties.schema, Properties.migrations)
    , (Timers.schema, Timers.migrations)
    , (Model.schema, Model.migrations)
    ]

schemaVersion :: Int64
schemaVersion = 2

type MigrationAction = ReaderT (RawSqlite SqlBackend) IO [EntityDef]

currentSchema :: Migration
currentSchema = mkMigration $ map fst
    (migrations :: [([EntityDef], Int64 -> MigrationAction)])

schemaUpdateForVersion :: MonadMigrate m => Int64 -> m Migration
schemaUpdateForVersion n = mkMigration <$> mapM (($n) . snd) migrations
