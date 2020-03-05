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

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.=))
import qualified Schema.Utils as Utils

import Schema.Dataset (DatasetId)
import Schema.Model (PredictionModelId)
import qualified Schema.ModelMetadata.V0 as V0

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
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

ModelTrainDataset
    modelId PredictionModelId
    datasetId DatasetId
    Primary modelId datasetId
    deriving Eq Show
|]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup [ 0 .= V0.schema, 18 .= schema ]
