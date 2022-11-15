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
module Schema.Variant where

import Data.String.Interpolate.IsString (i)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)
import Database.Persist.Types

import Pretty.Fields.Persistent
import Schema.Import
import Schema.Utils (Int64, MonadSql, Transaction, (.>), (.=))
import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import Schema.Graph (GraphId)
import Schema.VariantConfig (VariantConfigId)
import qualified Schema.Variant.V0 as V0
import qualified Schema.Variant.V1 as V1
import qualified Schema.Variant.V2 as V2

Utils.mkEntities "schema'" [persistUpperCase|
Variant
    graphId GraphId
    variantConfigId VariantConfigId
    algorithmId AlgorithmId
    result Hash Maybe
    maxStepId Int
    propsStored Bool
    retryCount Int
    UniqVariant graphId variantConfigId
    deriving Eq Show
|]

deriving instance Show (Unique Variant)

instance PrettyFields (Entity Variant) where
    prettyFieldInfo = ("Id", idField VariantId) :|
        [ ("Variant Config", namedIdField VariantVariantConfigId)
        , ("Algorithm", namedIdField VariantAlgorithmId)
        , ("Graph", namedIdField VariantGraphId)
        , ("Properties Stored", VariantPropsStored `fieldVia` prettyShow)
        , ("Retries", VariantRetryCount `fieldVia` prettyShow)
        , ("Result Hash", VariantResult `maybeFieldVia` prettyShow)
        ]

instance Importable Variant where
    updateFields =
        [ ForeignKeyField VariantGraphId
        , ForeignKeyField VariantAlgorithmId
        , ForeignKeyField VariantVariantConfigId
        ]

schema :: [EntityDef]
schema = Utils.addForeignRef "Variant" variantConfig schema'
  where
    variantConfig :: ForeignDef
    variantConfig = Utils.mkForeignRef "VariantConfig"
        [ ("variantConfigId", "id"), ("algorithmId", "algorithmId") ]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .= V0.schema
    , 7 .= V1.schema
    , 9 .> V2.schema $ do
        Utils.executeSql [i|
ALTER TABLE "Variant" ADD COLUMN "algorithmId" INTEGER REFERENCES "Algorithm"
|]

        Utils.executeSql [i|
REPLACE INTO "Variant"
SELECT Variant.id
     , Variant.graphId
     , Variant.variantConfigId
     , Variant.result
     , Variant.propsStored
     , Variant.retryCount
     , VariantConfig.algorithmId
FROM Variant

INNER JOIN VariantConfig
ON Variant.variantConfigId = VariantConfig.id
|]
    , 15 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "Variant" ADD COLUMN "maxStepId" INTEGER DEFAULT 0
|]

        Utils.executeSql [i|
REPLACE INTO "Variant"
SELECT Variant.id
     , Variant.graphId
     , Variant.variantConfigId
     , Variant.algorithmId
     , Variant.result
     , Variant.propsStored
     , Variant.retryCount
     , MaxStep.stepId
FROM Variant

INNER JOIN (
    SELECT Run.variantId, MAX(StepTimer.stepId) AS stepId
    FROM Run

    INNER JOIN StepTimer
    ON StepTimer.runId = Run.id

    GROUP BY Run.variantId
) AS MaxStep
ON MaxStep.variantId = Variant.id
|]
    ]
