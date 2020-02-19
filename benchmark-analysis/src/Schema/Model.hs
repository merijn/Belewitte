{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Pretty.Fields
import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.>))
import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import Schema.Platform (PlatformId)
import qualified Schema.Model.V0 as V0
import qualified Schema.Model.V1 as V1
import qualified Schema.Model.V2 as V2
import qualified Schema.Model.V3 as V3

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
PredictionModel
    platformId PlatformId
    algorithmId AlgorithmId
    algorithmVersion CommitId
    name Text
    prettyName Text Maybe
    description Text Maybe
    model Model
    legacyTrainFraction Double
    trainGraphs Percentage
    trainVariants Percentage
    trainSteps Percentage
    trainSeed Int64
    totalUnknownCount Int
    timestamp UTCTime
    UniqModel name
|]

instance PrettyFields PredictionModel where
    prettyFieldInfo = ("Id", idField PredictionModelId) :|
        [ ("Name", textField PredictionModelName)
        , ("Pretty Name", maybeTextField PredictionModelPrettyName)
        , ("Description", multilineTextField PredictionModelDescription)
        , ("Algorithm", idField PredictionModelAlgorithmId)
        , ("Platform", idField PredictionModelPlatformId)
        , ("Seed", PredictionModelTrainSeed `fieldVia` prettyShow)
        , ("Legacy", PredictionModelLegacyTrainFraction `fieldVia` flip percent 1)
        , ("Graphs", PredictionModelTrainGraphs `fieldVia` renderPercentage)
        , ("Variants", PredictionModelTrainVariants `fieldVia` renderPercentage)
        , ("Steps", PredictionModelTrainSteps `fieldVia` renderPercentage)
        , ("Unknown", PredictionModelTotalUnknownCount `fieldVia` prettyShow)
        , ("Algorithm Commit", PredictionModelAlgorithmVersion `fieldVia` getCommitId)
        ]

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
    , 9 .> V2.schema $ do
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
    , 17 .> V3.schema $ do
        Utils.executeSql [i|
ALTER TABLE "PredictionModel"
ADD COLUMN "algorithmVersion" TEXT
|]

        Utils.executeSql [i|
REPLACE INTO "PredictionModel"
SELECT PredictionModel.id
     , PredictionModel.platformId
     , PredictionModel.algorithmId
     , PredictionModel.name
     , PredictionModel.prettyName
     , PredictionModel.description
     , PredictionModel.model
     , PredictionModel.trainFraction
     , PredictionModel.trainSeed
     , PredictionModel.totalUnknownCount
     , PredictionModel.timestamp
     , CASE WHEN Derived.version IS NULL THEN 'Unknown'
            ELSE Derived.version
       END
FROM PredictionModel

INNER JOIN (
    SELECT PredictionModel.id
         , check_unique(DISTINCT RunConfig.algorithmVersion) AS version
    FROM PredictionModel

    INNER JOIN RunConfig
    ON PredictionModel.platformId = RunConfig.platformId
    AND PredictionModel.algorithmId = RunConfig.algorithmId

    INNER JOIN Run
    ON RunConfig.id = Run.runConfigId
    AND Run.timestamp < PredictionModel.timestamp
    AND Run.validated

    GROUP BY PredictionModel.id
) AS Derived
ON Derived.id = PredictionModel.id
|]
    , 19 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "PredictionModel"
RENAME COLUMN "trainFraction" TO "legacyTrainFraction"
|]

        Utils.executeSql [i|
ALTER TABLE "PredictionModel"
ADD COLUMN "trainGraphs" REAL
|]

        Utils.executeSql [i|
ALTER TABLE "PredictionModel"
ADD COLUMN "trainVariants" REAL
|]

        Utils.executeSql [i|
ALTER TABLE "PredictionModel"
ADD COLUMN "trainSteps" REAL
|]

        Utils.executeSql [i|
REPLACE INTO "PredictionModel"
SELECT PredictionModel.id
     , PredictionModel.platformId
     , PredictionModel.algorithmId
     , PredictionModel.algorithmVersion
     , PredictionModel.name
     , PredictionModel.prettyName
     , PredictionModel.description
     , PredictionModel.model
     , PredictionModel.legacyTrainFraction
     , PredictionModel.trainSeed
     , PredictionModel.totalUnknownCount
     , PredictionModel.timestamp
     , CASE PredictionModel.legacyTrainFraction
         WHEN 0 THEN 1.0
         ELSE 0.0
       END
     , CASE PredictionModel.legacyTrainFraction
         WHEN 0 THEN 1.0
         ELSE 0.0
       END
     , CASE PredictionModel.legacyTrainFraction
         WHEN 0 THEN 1.0
         ELSE 0.0
       END
FROM PredictionModel
|]
    ]
