{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Model where

import Control.Monad (when)
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)

import Model (Model)
import Pretty.Fields.Persistent
import Schema.Utils
    ( Entity
    , EntityDef
    , Int64
    , MonadLogger
    , MonadSql
    , MonadThrow
    , Transaction
    , (.>)
    )
import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import Schema.Platform (PlatformId)
import qualified Schema.Model.V0 as V0
import qualified Schema.Model.V1 as V1
import qualified Schema.Model.V2 as V2
import qualified Schema.Model.V3 as V3
import qualified Schema.Model.V4 as V4
import qualified Schema.Model.V5 as V5
import qualified Schema.Model.V6 as V6

Utils.mkEntities "schema" [persistUpperCase|
PredictionModel
    platformId PlatformId
    algorithmId AlgorithmId
    algorithmVersion CommitId
    name Text
    prettyName Text Maybe
    description Text Maybe
    model Model
    skipIncomplete Bool
    allowNewer AllowNewer
    legacyTrainFraction Double
    trainGraphs Percentage
    trainVariants Percentage
    trainSteps Percentage
    trainSeed Int64
    totalUnknownCount Int
    timestamp UTCTime
    UniqModel name
|]

deriving instance Show (Unique PredictionModel)

instance PrettyFields (Entity PredictionModel) where
    prettyFieldInfo = ("Id", idField PredictionModelId) :|
        [ ("Name", textField PredictionModelName)
        , ("Pretty Name", maybeTextField PredictionModelPrettyName)
        , ("Description", multilineTextField PredictionModelDescription)
        , ("Algorithm", namedIdField PredictionModelAlgorithmId)
        , ("Platform", namedIdField PredictionModelPlatformId)
        , ("Skip", PredictionModelSkipIncomplete `fieldVia` prettyShow)
        , ("Newer", PredictionModelAllowNewer `fieldVia` prettyShow)
        , ("Seed", PredictionModelTrainSeed `fieldVia` prettyShow)
        , ("Legacy", PredictionModelLegacyTrainFraction `fieldVia` flip percent 1)
        , ("Graphs", PredictionModelTrainGraphs `fieldVia` renderPercentage)
        , ("Variants", PredictionModelTrainVariants `fieldVia` renderPercentage)
        , ("Steps", PredictionModelTrainSteps `fieldVia` renderPercentage)
        , ("Unknown", PredictionModelTotalUnknownCount `fieldVia` prettyShow)
        , ("Algorithm Commit", PredictionModelAlgorithmVersion `fieldVia` getCommitId)
        ]

instance NamedEntity PredictionModel where
    entityName =
        optionalPrettyName predictionModelPrettyName predictionModelName

migrations
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .> V0.schema $ do
        modelTimeStamp <- (/= (0 :: Int)) <$> Utils.executeSqlSingleValue [i|
SELECT COUNT(*)
FROM pragma_table_info('PredictionModel')
WHERE name = 'timestamp'
|]

        when modelTimeStamp $ do
            Utils.executeSql [i|
UPDATE "PredictionModel"
SET timestamp = strftime('%Y-%m-%dT%H:%M:%f',replace(timestamp,' UTC',''))
|]

    , 1 .> V1.schema $ do
        Utils.executeSql [i|
ALTER TABLE "PredictionModel" RENAME COLUMN "gpuId" TO "platformId" |]
    , 2 .> V2.schema $ do
        Utils.executeSql [i|
ALTER TABLE "PredictionModel" ADD COLUMN "name" VARCHAR
|]
        Utils.executeSql [i|
UPDATE 'PredictionModel' SET 'name' = "Model-" || id
|]
    , 9 .> V3.schema $ do
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
    , 17 .> V4.schema $ do
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
    , 19 .> V5.schema $ do
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
    , 23 .> V6.schema $ do
        Utils.executeSql [i|
ALTER TABLE "PredictionModel"
ADD COLUMN "skipIncomplete" BOOLEAN NOT NULL DEFAULT 0
|]
    , 27 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "PredictionModel"
ADD COLUMN "allowNewer" VARCHAR NOT NULL DEFAULT 'NoNewer'
|]
    ]
