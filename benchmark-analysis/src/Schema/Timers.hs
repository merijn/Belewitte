{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
module Schema.Timers where

import Control.Monad (when)
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)

import Pretty.Fields.Persistent
import Schema.Utils
    ( Entity
    , EntityDef
    , ForeignDef
    , Int64
    , MonadLogger
    , MonadSql
    , MonadThrow
    , Transaction
    , (.>)
    )
import qualified Schema.Utils as Utils
import qualified Schema.Timers.V0 as V0
import qualified Schema.Timers.V1 as V1
import qualified Schema.Timers.V2 as V2

import Schema.Run (RunId)
import Schema.Variant (VariantId)

Utils.mkEntities "schema'" [persistUpperCase|
TotalTimer
    runId RunId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    Primary runId name
    deriving Eq Show

StepTimer
    runId RunId
    variantId VariantId
    stepId Int
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    Primary runId stepId name
    deriving Eq Show
|]

instance PrettyFields (Entity TotalTimer) where
    prettyFieldInfo = ("Run", idField TotalTimerRunId) :|
        [ ("Name", textField TotalTimerName)
        , ("Min. Time", doubleField 0 TotalTimerMinTime)
        , ("Avg. Time", doubleField 0 TotalTimerAvgTime)
        , ("Max Time", doubleField 0 TotalTimerMaxTime)
        , ("Std. Dev.", doubleField_ TotalTimerStdDev)
        ]

instance NamedEntity TotalTimer where
    entityName = totalTimerName

instance PrettyFields (Entity StepTimer) where
    prettyFieldInfo = ("Run", idField StepTimerRunId) :|
        [ ("Variant", idField StepTimerVariantId)
        , ("Step", StepTimerStepId `fieldVia` prettyShow)
        , ("Name", textField StepTimerName)
        , ("Min. Time", doubleField 0 StepTimerMinTime)
        , ("Avg. Time", doubleField 0 StepTimerAvgTime)
        , ("Max Time", doubleField 0 StepTimerMaxTime)
        , ("Std. Dev.", doubleField_ StepTimerStdDev)
        ]

instance NamedEntity StepTimer where
    entityName = stepTimerName

schema :: [EntityDef]
schema = Utils.addForeignRef "StepTimer" run $ schema'
  where
    run :: ForeignDef
    run = Utils.mkForeignRef "Run"
        [ ("runId", "id"), ("variantId", "variantId") ]

migrations
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .> V0.schema $ do
        stepTimeStamp <- (/= (0 :: Int)) <$> Utils.executeSqlSingleValue [i|
SELECT COUNT(*)
FROM pragma_table_info('StepTimer')
WHERE name = 'timestamp'
|]
        when stepTimeStamp $ do
            Utils.executeSql [i|
UPDATE "StepTimer"
SET timestamp = strftime('%Y-%m-%dT%H:%M:%f',replace(timestamp,' UTC',''))
|]

        totalTimeStamp <- (/= (0 :: Int)) <$> Utils.executeSqlSingleValue [i|
SELECT COUNT(*)
FROM pragma_table_info('TotalTimer')
WHERE name = 'timestamp'
|]

        when totalTimeStamp $ do
            Utils.executeSql [i|
UPDATE "TotalTimer"
SET timestamp = strftime('%Y-%m-%dT%H:%M:%f',replace(timestamp,' UTC',''))
|]

    , 1 .> V1.schema $ do
        Utils.executeSql [i|
ALTER TABLE "TotalTimer" RENAME COLUMN "gpuId" TO "platformId"
|]
        Utils.executeSql [i|
ALTER TABLE "StepTimer" RENAME COLUMN "gpuId" TO "platformId"
|]
    , 6 .> V2.schema $ do
        Utils.executeSql [i|
ALTER TABLE "TotalTimer"
ADD COLUMN "runId" INTEGER REFERENCES "Run"
|]

        Utils.executeSql [i|
ALTER TABLE "StepTimer"
ADD COLUMN "runId" INTEGER REFERENCES "Run"
|]

        Utils.executeSql [i|
REPLACE INTO "TotalTimer"
SELECT TotalTimer.platformId
     , TotalTimer.variantId
     , TotalTimer.implId
     , TotalTimer.name
     , TotalTimer.minTime
     , TotalTimer.avgTime
     , TotalTimer.maxTime
     , TotalTimer.stdDev
     , TotalTimer.timestamp
     , TotalTimer.wrongResult
     , Run.id
FROM TotalTimer

INNER JOIN Run
ON  Run.variantId = TotalTimer.variantId
AND Run.implId = TotalTimer.implId

INNER JOIN Variant
ON Variant.id = TotalTimer.variantId

INNER JOIN Graph
ON Graph.id = Variant.graphId

INNER JOIN RunConfig
ON  RunConfig.id = Run.runConfigId
AND RunConfig.platformId = TotalTimer.platformId
AND RunConfig.algorithmId = Variant.algorithmId
AND RunConfig.datasetId = Graph.datasetId
|]

        Utils.executeSql [i|
REPLACE INTO "StepTimer"
SELECT StepTimer.platformId
     , StepTimer.variantId
     , StepTimer.stepId
     , StepTimer.implId
     , StepTimer.name
     , StepTimer.minTime
     , StepTimer.avgTime
     , StepTimer.maxTime
     , StepTimer.stdDev
     , StepTimer.timestamp
     , StepTimer.wrongResult
     , Run.id
FROM StepTimer

INNER JOIN Run
ON  Run.variantId = StepTimer.variantId
AND Run.implId = StepTimer.implId

INNER JOIN Variant
ON Variant.id = StepTimer.variantId

INNER JOIN Graph
ON Graph.id = Variant.graphId

INNER JOIN RunConfig
ON  RunConfig.id = Run.runConfigId
AND RunConfig.platformId = StepTimer.platformId
AND RunConfig.algorithmId = Variant.algorithmId
AND RunConfig.datasetId = Graph.datasetId
|]
    , 22 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "StepTimer"
ADD COLUMN "variantId" INTEGER REFERENCES "Variant"
|]

        Utils.executeSql [i|
REPLACE INTO "StepTimer"
SELECT StepTimer.runId
     , StepTimer.stepId
     , StepTimer.name
     , StepTimer.minTime
     , StepTimer.avgTime
     , StepTimer.maxTime
     , StepTimer.stdDev
     , Run.variantId
FROM StepTimer
INNER JOIN Run
ON Run.id = StepTimer.runId
|]
    ]
