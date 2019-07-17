{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Model where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Model (Model)
import Schema.Utils (EntityDef, Int64, MonadMigrate, (.=))
import qualified Schema.Utils as Utils

import Schema.Implementation (ImplementationId)
import Schema.Platform (PlatformId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
PredictionModel
    platformId PlatformId
    name Text
    prettyName Text Maybe
    description Text Maybe
    model Model
    trainFraction Double
    trainSeed Int
    totalUnknownCount Int
    UniqModel name
    timestamp UTCTime

ModelGraphProperty
    modelId PredictionModelId
    property Text
    importance Double
    Primary modelId property
    deriving Eq Show

ModelStepProperty
    modelId PredictionModelId
    property Text
    importance Double
    Primary modelId property
    deriving Eq Show

UnknownPrediction
    modelId PredictionModelId
    count Int
    deriving Eq Show

UnknownSet
    unknownPredId UnknownPredictionId
    implId ImplementationId
    Primary unknownPredId implId
    deriving Eq Show
|]

migrations :: MonadMigrate m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup schema
    [ 0 .= schema $ do
        Utils.executeMigrationSql [i|
ALTER TABLE 'PredictionModel' RENAME COLUMN 'gpuId' TO 'platformId'
|]
    , 1 .= schema $ do
        Utils.executeMigrationSql [i|
ALTER TABLE 'PredictionModel' ADD COLUMN 'name' VARCHAR
|]
        Utils.executeMigrationSql [i|
UPDATE 'PredictionModel' SET 'name' = "Model-" || id
|]
    ]
