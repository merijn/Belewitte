{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.ModelMetadata where

import Data.String.Interpolate.IsString (i)
import Database.Persist.TH (persistUpperCase)

import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.=), (.>))
import qualified Schema.Utils as Utils

import Schema.Dataset (DatasetId)
import Schema.Model (PredictionModelId)
import Schema.Properties (PropertyNameId)
import qualified Schema.ModelMetadata.V0 as V0
import qualified Schema.ModelMetadata.V1 as V1
import qualified Schema.ModelMetadata.V2 as V2

Utils.mkEntities "schema" [persistUpperCase|
ModelProperty
    modelId PredictionModelId
    propId PropertyNameId
    propertyIdx Int
    importance Double
    Primary modelId propId
    deriving Eq Show

ModelTrainDataset
    modelId PredictionModelId
    datasetId DatasetId
    Primary modelId datasetId
    deriving Eq Show
|]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .= V0.schema
    , 18 .= V1.schema
    , 24 .= V2.schema
    , 25 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "ModelProperty" ADD COLUMN "propertyIdx" INTEGER
|]

        Utils.executeSql [i|
REPLACE INTO "ModelProperty"
SELECT modelId, propId, importance
     , ROW_NUMBER() OVER (PARTITION BY modelId ORDER BY propId) - 1
FROM ModelProperty
|]
    , 26 .> schema $ do
        -- Cleanup leftover tables from earlier migrations
        Utils.executeSql [i|DROP TABLE IF EXISTS ModelGraphProperty|]
        Utils.executeSql [i|DROP TABLE IF EXISTS ModelStepProperty|]
    ]
