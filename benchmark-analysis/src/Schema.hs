{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Schema
    ( ByteString
    , CommitId(..)
    , GlobalVars.GlobalVar(..)
    , Hash(..)
    , HashDigest
    , ImplType(..)
    , Model
    , Percentage(..)
    , PersistValue(..)
    , toPersistValue
    , Text
    , module Schema.Algorithm
    , module Schema.Dataset
    , module Schema.External
    , module Schema.Graph
    , module Schema.Implementation
    , module Schema.Model
    , module Schema.ModelMetadata
    , module Schema.Platform
    , module Schema.Properties
    , module Schema.Run
    , module Schema.RunConfig
    , module Schema.Timers
    , module Schema.UnknownPredictions
    , module Schema.Variant
    , module Schema.VariantConfig
    , optimalImplId
    , bestNonSwitchingImplId
    , predictedImplId
    , getAlgoName
    , getImplName
    , getExternalName
    , mkPercentage
    , percent
    , renderPercentage
    , toImplNames
    , schemaVersion
    , currentSchema
    , updateSchemaToVersion
    , updateIndicesToVersion
    , validRational
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Persist.Sql
    (EntityDef, Migration, PersistValue(..), toPersistValue)

import Model (Model)
import Schema.Utils (mkMigration)
import Sql.Core (DummySql, MonadSql, Transaction)
import Types
import Utils.Pair (Pair, toPair)

import Schema.Algorithm hiding (migrations, schema)
import qualified Schema.Algorithm as Algorithm
import Schema.Dataset hiding (migrations, schema)
import qualified Schema.Dataset as Dataset
import Schema.External hiding (migrations, schema, schema')
import qualified Schema.External as External
import qualified Schema.GlobalVars as GlobalVars
import Schema.Graph hiding (migrations, schema)
import qualified Schema.Graph as Graph
import Schema.Implementation hiding (migrations, schema)
import qualified Schema.Implementation as Implementation
import Schema.Indices (updateIndicesToVersion)
import Schema.Model hiding (migrations, schema)
import qualified Schema.Model as Model
import Schema.ModelMetadata hiding (migrations, schema)
import qualified Schema.ModelMetadata as ModelMetadata
import Schema.Platform hiding (migrations, schema)
import qualified Schema.Platform as Platform
import Schema.Properties hiding (migrations, schema)
import qualified Schema.Properties as Properties
import Schema.Run hiding (migrations, schema, schema')
import qualified Schema.Run as Run
import Schema.RunConfig hiding (migrations, schema)
import qualified Schema.RunConfig as RunConfig
import Schema.Timers hiding (migrations, schema, schema')
import qualified Schema.Timers as Timers
import Schema.UnknownPredictions hiding (migrations, schema, schema')
import qualified Schema.UnknownPredictions as UnknownPredictions
import Schema.Variant hiding (migrations, schema, schema')
import qualified Schema.Variant as Variant
import Schema.VariantConfig hiding (migrations, schema)
import qualified Schema.VariantConfig as VariantConfig
import Schema.Version (schemaVersion)

optimalImplId :: Integral n => n
optimalImplId = -1

bestNonSwitchingImplId :: Integral n => n
bestNonSwitchingImplId = -2

predictedImplId :: Integral n => n
predictedImplId = -3

getAlgoName :: Algorithm -> Text
getAlgoName Algorithm{algorithmName,algorithmPrettyName} =
  fromMaybe algorithmName algorithmPrettyName

getImplName :: Implementation -> Text
getImplName Implementation{implementationName,implementationPrettyName} =
  fromMaybe implementationName implementationPrettyName

getExternalName :: ExternalImpl -> Text
getExternalName ExternalImpl{externalImplName,externalImplPrettyName} =
  fromMaybe externalImplName externalImplPrettyName

toImplNames
    :: (IntMap Implementation -> IntMap Implementation)
    -> (IntMap ExternalImpl -> IntMap ExternalImpl)
    -> (IntMap Implementation, IntMap ExternalImpl)
    -> Pair (IntMap Text)
toImplNames f g = toPair (fmap getImplName . f) (fmap getExternalName . g)

migrations :: MonadSql m => [([EntityDef], Int64 -> Transaction m [EntityDef])]
migrations =
    [ (Platform.schema, Platform.migrations)
    , (Dataset.schema, Dataset.migrations)
    , (Graph.schema, Graph.migrations)
    , (Algorithm.schema, Algorithm.migrations)
    , (External.schema, External.migrations)
    , (Implementation.schema, Implementation.migrations)
    , (Variant.schema, Variant.migrations)
    , (Properties.schema, Properties.migrations)
    , (RunConfig.schema, RunConfig.migrations)
    , (Run.schema, Run.migrations)
    , (Timers.schema, Timers.migrations)
    , (Model.schema, Model.migrations)
    , (ModelMetadata.schema, ModelMetadata.migrations)
    , (VariantConfig.schema, VariantConfig.migrations)
    , (UnknownPredictions.schema, UnknownPredictions.migrations)
    , (GlobalVars.schema, GlobalVars.migrations)
    ]

type MigrationAction = Transaction DummySql [EntityDef]

currentSchema :: Migration
currentSchema = mkMigration $ map fst
    (migrations :: [([EntityDef], Int64 -> MigrationAction)])

updateSchemaToVersion :: MonadSql m => Int64 -> Transaction m Migration
updateSchemaToVersion n = mkMigration <$> mapM (($n) . snd) migrations
