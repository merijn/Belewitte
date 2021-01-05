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
module Schema.ModelMetadata.V1 where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Dataset (DatasetId)
import Schema.Model (PredictionModelId)

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
