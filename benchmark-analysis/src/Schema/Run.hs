{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Run where

import Data.String.Interpolate.IsString (i)
import Data.Time.Clock (UTCTime)
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Utils (EntityDef, Int64, MonadSql, (.>))
import qualified Schema.Utils as Utils

import Schema.Implementation (ImplementationId)
import Schema.RunConfig (RunConfigId)
import Schema.Variant (VariantId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Run
    runConfigId RunConfigId
    variantId VariantId
    implId ImplementationId
    timestamp UTCTime
    validated Bool
    UniqRun runConfigId variantId implId
    deriving Eq Show
|]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 6 .> schema $ do
        Utils.createTableFromSchema schema

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
         , MAX(MAX(TotalTimer.timestamp, StepTimer.timestamp)) AS timestamp
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

    INNER JOIN StepTimer
    ON  StepTimer.platformId = RunConfig.platformId
    AND StepTimer.variantId = Variant.id
    AND StepTimer.implId = TotalTimer.implid

    GROUP BY RunConfig.id, Variant.id, TotalTimer.implId
)
|]
    ]
