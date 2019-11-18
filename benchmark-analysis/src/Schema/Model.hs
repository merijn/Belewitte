{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Platform (PlatformId)
import qualified Schema.Model.V0 as V0
import qualified Schema.Model.V1 as V1

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
PredictionModel
    platformId PlatformId
    algorithmId AlgorithmId
    name Text
    prettyName Text Maybe
    description Text Maybe
    model Model
    trainFraction Double
    trainSeed Int
    totalUnknownCount Int
    timestamp UTCTime
    UniqModel name
|]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 1 .> V0.schema $ do
        Utils.executeSql [i|
ALTER TABLE "PredictionModel" RENAME COLUMN "gpuId" TO "platformId" |]
    , 2 .> V1.schema $ do
        Utils.executeSql [i|
ALTER TABLE "PredictionModel" ADD COLUMN "name" VARCHAR
|]
        Utils.executeSql [i|
UPDATE 'PredictionModel' SET 'name' = "Model-" || id
|]
    , 9 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "PredictionModel"
ADD COLUMN "algorithmId" INTEGER REFERENCES "Algorithm"
|]

        Utils.executeSql [i|
REPLACE INTO "PredictionModel"
SELECT PredictionModel.id
     , PredictionModel.platformId
     , PredictionModel.name
     , PredictionModel.prettyName
     , PredictionModel.description
     , PredictionModel.model
     , PredictionModel.trainFraction
     , PredictionModel.trainSeed
     , PredictionModel.totalUnknownCount
     , PredictionModel.timestamp
     , AlgorithmMapping.algorithmId
FROM PredictionModel

INNER JOIN
(   SELECT UnknownPrediction.modelId, algorithmId
    FROM UnknownPrediction

    INNER JOIN UnknownSet
    ON UnknownPrediction.id = UnknownSet.unknownPredId

    INNER JOIN Implementation
    ON UnknownSet.implId = Implementation.id

    GROUP BY modelId, algorithmId
) AS AlgorithmMapping
ON PredictionModel.id = AlgorithmMapping.modelId
|]
    ]
