{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Run where

import Data.String.Interpolate.IsString (i)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (Unique)

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

-- Schema versions for Run table initialisation
import qualified Schema.Dataset as Dataset
import qualified Schema.Graph.V2 as Graph.V2
import qualified Schema.Implementation.V0 as Implementation.V0
import qualified Schema.Platform.V1 as Platform.V1
import qualified Schema.RunConfig.V0 as RunConfig.V0
import qualified Schema.Variant.V0 as Variant.V0

-- Schema versions for current schema
import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Implementation (ImplementationId)
import qualified Schema.Implementation as Implementation
import Schema.RunConfig (RunConfigId)
import qualified Schema.RunConfig as RunConfig
import Schema.Variant (VariantId)
import qualified Schema.Variant as Variant
import qualified Schema.Run.V0 as V0

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Implementation.schema, RunConfig.schema, Variant.schema]
    [Utils.mkSchema|
Run
    runConfigId RunConfigId
    variantId VariantId
    implId ImplementationId
    algorithmId AlgorithmId
    timestamp UTCTime
    validated Bool
    UniqRun runConfigId variantId implId algorithmId
    Foreign RunConfig foreignRunConfig runConfigId algorithmId References Id algorithmId
    Foreign Variant foreignVariant variantId algorithmId References Id algorithmId
    Foreign Implementation foreignImplementation implId algorithmId References Id algorithmId
    deriving Eq Show
|]

deriving instance Show (Unique Run)

instance PrettyFields (Entity Run) where
    prettyFieldInfo = ("Id", idField RunId) :|
        [ ("Run Config", idField RunRunConfigId)
        , ("Algorithm", namedIdField RunAlgorithmId)
        , ("Variant", idField RunVariantId)
        , ("Implementation", namedIdField RunImplId)
        , ("Validated", RunValidated `fieldVia` prettyShow)
        , ("Timestamp", RunTimestamp `fieldVia` prettyShow)
        ]

migrations
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 6 .> V0.schema $ do
        Utils.createTableFromSchema
            [ Algorithm.schema
            , Dataset.schema
            , Graph.V2.schema
            , Implementation.V0.schema
            , Platform.V1.schema
            , RunConfig.V0.schema
            , Variant.V0.schema
            , V0.schema
            ]

        Utils.executeSql [i|
INSERT INTO "Run"
SELECT ROW_NUMBER() OVER (ORDER BY runConfigId, variantId, implId)
     , runConfigId
     , variantId
     , implId
     , timestamp
     , validated
FROM (
    SELECT RunConfig.id AS runConfigId
         , Variant.id AS variantId
         , TotalTimer.implId
         , MAX(MAX
            ( IFNULL(TotalTimer.timestamp, strftime('%Y-%m-%dT%H:%M:%f', "now"))
            , IFNULL(StepTimer.timestamp, strftime('%Y-%m-%dT%H:%M:%f', "now"))
            )) AS timestamp
         , MIN(TotalTimer.wrongResult ISNULL AND StepTimer.wrongResult ISNULL)
           AS validated
    FROM RunConfig

    INNER JOIN Graph
    ON Graph.datasetId = RunConfig.datasetId

    INNER JOIN Variant
    ON Variant.algorithmId = RunConfig.algorithmId
    AND Variant.graphId = Graph.id

    INNER JOIN TotalTimer
    ON  TotalTimer.platformId = RunConfig.platformId
    AND TotalTimer.variantId = Variant.id

    LEFT JOIN StepTimer
    ON  StepTimer.platformId = RunConfig.platformId
    AND StepTimer.variantId = Variant.id
    AND StepTimer.implId = TotalTimer.implid

    GROUP BY RunConfig.id, Variant.id, TotalTimer.implId
)
|]
    , 9 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "Run" ADD COLUMN "algorithmId" INTEGER REFERENCES "Algorithm"
|]

        Utils.executeSql [i|
REPLACE INTO "Run"
SELECT Run.id
     , Run.runConfigId
     , Run.variantId
     , Run.implId
     , Run.timestamp
     , Run.validated
     , RunConfig.algorithmId
FROM Run

INNER JOIN RunConfig
ON Run.runConfigId = RunConfig.id
|]
    ]
