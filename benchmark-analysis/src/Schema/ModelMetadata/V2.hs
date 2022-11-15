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

import Database.Persist.TH (persistUpperCase)

import qualified Schema.Utils as Utils

import Schema.Dataset (DatasetId)
import Schema.Model (PredictionModelId)
import Schema.Properties (PropertyNameId)

Utils.mkEntities "schema" [persistUpperCase|
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
