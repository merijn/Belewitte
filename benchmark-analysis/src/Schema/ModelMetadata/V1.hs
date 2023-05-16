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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.ModelMetadata.V1 where

import Data.Text (Text)

import qualified Schema.Utils as Utils

import Schema.Dataset (DatasetId)
import qualified Schema.Dataset as Dataset
import Schema.Model (PredictionModelId)
import qualified Schema.Model as Model

Utils.mkEntitiesWith "schema" [Dataset.schema, Model.schema] [Utils.mkSchema|
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
