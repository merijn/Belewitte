{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.ModelMetadata.V2 where

import qualified Schema.Utils as Utils

import Schema.Dataset (DatasetId)
import qualified Schema.Dataset as Dataset
import Schema.Model (PredictionModelId)
import qualified Schema.Model as Model
import Schema.Properties (PropertyNameId)
import qualified Schema.Properties as Property

Utils.mkEntitiesWith "schema" 
    [Dataset.schema, Model.schema, Property.schema] [Utils.mkSchema|
ModelProperty
    modelId PredictionModelId
    propId PropertyNameId
    importance Double
    Primary modelId propId
    deriving Eq Show

ModelTrainDataset
    modelId PredictionModelId
    datasetId DatasetId
    Primary modelId datasetId
    deriving Eq Show
|]
