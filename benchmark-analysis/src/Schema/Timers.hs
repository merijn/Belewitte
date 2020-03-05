{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Timers where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Fields
import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.>))
import qualified Schema.Utils as Utils
import qualified Schema.Timers.V0 as V0

import Schema.Run (RunId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
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
    stepId Int
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    Primary runId stepId name
    deriving Eq Show
|]

instance PrettyFields TotalTimer where
    prettyFieldInfo = ("Run", idField TotalTimerRunId) :|
        [ ("Name", textField TotalTimerName)
        , ("Min. Time", TotalTimerMinTime `fieldVia` prettyDouble)
        , ("Avg. Time", TotalTimerAvgTime `fieldVia` prettyDouble)
        , ("Max Time", TotalTimerMaxTime `fieldVia` prettyDouble)
        , ("Std. Dev.", TotalTimerStdDev `fieldVia` prettyDouble)
        ]

instance PrettyFields StepTimer where
    prettyFieldInfo = ("Run", idField StepTimerRunId) :|
        [ ("Step", StepTimerStepId `fieldVia` prettyShow)
        , ("Name", textField StepTimerName)
        , ("Min. Time", StepTimerMinTime `fieldVia` prettyDouble)
        , ("Avg. Time", StepTimerAvgTime `fieldVia` prettyDouble)
        , ("Max Time", StepTimerMaxTime `fieldVia` prettyDouble)
        , ("Std. Dev.", StepTimerStdDev `fieldVia` prettyDouble)
        ]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 1 .> V0.schema $ do
        Utils.executeSql [i|
ALTER TABLE 'TotalTimer' RENAME COLUMN 'gpuId' TO 'platformId'
|]
        Utils.executeSql [i|
ALTER TABLE 'StepTimer' RENAME COLUMN 'gpuId' TO 'platformId'
|]
    , 6 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE 'TotalTimer'
ADD COLUMN 'runId' INTEGER REFERENCES 'Run'
|]

        Utils.executeSql [i|
ALTER TABLE 'StepTimer'
ADD COLUMN 'runId' INTEGER REFERENCES 'Run'
|]

        Utils.executeSql [i|
REPLACE INTO 'TotalTimer'
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
REPLACE INTO 'StepTimer'
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
    ]
