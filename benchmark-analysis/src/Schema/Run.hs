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
import Database.Persist.TH (persistUpperCase)

import Pretty.Fields.Persistent
import Schema.Utils
    (Entity, EntityDef, ForeignDef, Int64, MonadSql, Transaction, (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Implementation (ImplementationId)
import Schema.RunConfig (RunConfigId)
import Schema.Variant (VariantId)
import qualified Schema.Run.V0 as V0

Utils.mkEntities "schema'" [persistUpperCase|
Run
    runConfigId RunConfigId
    variantId VariantId
    implId ImplementationId
    algorithmId AlgorithmId
    timestamp UTCTime
    validated Bool
    UniqRun runConfigId variantId implId algorithmId
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

schema :: [EntityDef]
schema = Utils.addForeignRef "Run" runConfig
       . Utils.addForeignRef "Run" variant
       . Utils.addForeignRef "Run" impl
       $ schema'
  where
    runConfig :: ForeignDef
    runConfig = Utils.mkForeignRef "RunConfig"
        [ ("runConfigId", "id"), ("algorithmId", "algorithmId") ]

    variant :: ForeignDef
    variant = Utils.mkForeignRef "Variant"
        [ ("variantId", "id"), ("algorithmId", "algorithmId") ]

    impl :: ForeignDef
    impl = Utils.mkForeignRef "Implementation"
        [ ("implId", "id"), ("algorithmId", "algorithmId") ]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 6 .> V0.schema $ do
        Utils.createTableFromSchema V0.schema

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
