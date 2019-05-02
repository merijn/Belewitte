{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema.Model where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Model (Model)
import Schema.Utils (Int64, MigrationAction, (.=), mkMigrationLookup)

import Schema.Implementation (ImplementationId)
import Schema.Platform (PlatformId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
PredictionModel
    platformId PlatformId
    model Model
    trainFraction Double
    trainSeed Int
    totalUnknownCount Int
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

migrations :: Int64 -> MigrationAction
migrations = mkMigrationLookup schema
    [ 0 .= schema $ do
        Sql.rawExecute [i|
ALTER TABLE 'PredictionModel' RENAME COLUMN 'gpuId' TO 'platformId'
|] []
    ]
