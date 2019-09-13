{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.External where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Utils (EntityDef, Int64, MonadSql, (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Platform (PlatformId)
import Schema.Variant (VariantId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
ExternalImpl
    algorithmId AlgorithmId
    name Text
    prettyName Text Maybe
    UniqExternalImpl algorithmId name
    deriving Eq Show

ExternalTimer
    platformId PlatformId
    variantId VariantId
    implId ExternalImplId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    timestamp UTCTime
    Primary platformId variantId implId name
    deriving Eq Show
|]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 4 .> schema $ do

        Utils.executeSql [i|
CREATE TABLE IF NOT EXISTS "ExternalImpl"
("id" INTEGER PRIMARY KEY
,"algorithmId" INTEGER NOT NULL REFERENCES "Algorithm"
,"name" VARCHAR NOT NULL
,"prettyName" VARCHAR NULL
,CONSTRAINT "UniqExternalImpl" UNIQUE ("algorithmId","name")
)
|]

        Utils.executeSql [i|
CREATE TABLE IF NOT EXISTS "ExternalTimer"
("platformId" INTEGER NOT NULL REFERENCES "Platform"
,"variantId" INTEGER NOT NULL REFERENCES "Variant"
,"implId" INTEGER NOT NULL REFERENCES "ExternalImpl"
,"name" VARCHAR NOT NULL
,"minTime" REAL NOT NULL
,"avgTime" REAL NOT NULL
,"maxTime" REAL NOT NULL
,"stdDev" REAL NOT NULL
,"timestamp" TIMESTAMP NOT NULL
,PRIMARY KEY ("platformId","variantId","implId","name")
)
|]

        Utils.executeSql [i|
INSERT INTO "ExternalImpl" SELECT id, algorithmId, name, prettyName
FROM Implementation WHERE type = "Comparison"
|]

        Utils.executeSql [i|
INSERT INTO "ExternalTimer"
SELECT TotalTimer.platformId, TotalTimer.variantId, TotalTimer.implId
     , TotalTimer.name, TotalTimer.minTime, TotalTimer.avgTime
     , TotalTimer.maxTime, TotalTimer.stdDev, TotalTimer.timestamp
FROM TotalTimer
INNER JOIN Implementation ON TotalTimer.implId = Implementation.id
WHERE Implementation.type = "Comparison"
|]

        Utils.executeSql [i|
DELETE FROM TotalTimer
WHERE TotalTimer.implId IN
    ( SELECT id FROM Implementation WHERE type = "Comparison")
|]

        Utils.executeSql [i|
DELETE FROM Implementation
WHERE type = "Comparison"
|]
    ]
