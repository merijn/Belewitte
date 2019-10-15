{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.External where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Columns
import Schema.Utils (EntityDef, ForeignDef, Int64, MonadSql, (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Platform (PlatformId)
import Schema.Variant (VariantId)
import qualified Schema.External.V0 as V0

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema'"] [persistUpperCase|
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
    algorithmId AlgorithmId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    Primary platformId variantId implId algorithmId name
    deriving Eq Show
|]

instance PrettyColumns ExternalImpl where
    prettyColumnInfo = idColumn ExternalImplId :|
        [ idColumn ExternalImplAlgorithmId
        , column ExternalImplName
        , maybeColumn ExternalImplPrettyName
        ]

instance PrettyColumns ExternalTimer where
    prettyColumnInfo = idColumn ExternalTimerPlatformId :|
        [ idColumn ExternalTimerVariantId
        , idColumn ExternalTimerImplId
        , idColumn ExternalTimerAlgorithmId
        , column ExternalTimerName
        , ExternalTimerMinTime `columnVia` prettyDouble
        , ExternalTimerAvgTime `columnVia` prettyDouble
        , ExternalTimerMaxTime `columnVia` prettyDouble
        , ExternalTimerStdDev `columnVia` prettyDouble
        ]

schema :: [EntityDef]
schema = Utils.addForeignRef "ExternalTimer" variant
       . Utils.addForeignRef "ExternalTimer" extImpl
       $ schema'
  where
    variant :: ForeignDef
    variant = Utils.mkForeignRef "Variant"
        [ ("variantId", "id"), ("algorithmId", "algorithmId") ]

    extImpl :: ForeignDef
    extImpl = Utils.mkForeignRef "ExternalImpl"
        [ ("implId", "id"), ("algorithmId", "algorithmId") ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 4 .> V0.schema $ do
        Utils.createTableFromSchema V0.schema

        Utils.executeSql [i|
INSERT INTO "ExternalImpl" SELECT id, algorithmId, name, prettyName
FROM Implementation WHERE type = "Comparison"
|]

        Utils.executeSql [i|
INSERT INTO "ExternalTimer"
SELECT TotalTimer.platformId, TotalTimer.variantId, TotalTimer.implId
     , TotalTimer.name, TotalTimer.minTime, TotalTimer.avgTime
     , TotalTimer.maxTime, TotalTimer.stdDev
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
    , 9 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "ExternalTimer"
ADD COLUMN "algorithmId" INTEGER REFERENCES "Algorithm"
|]

        Utils.executeSql [i|
REPLACE INTO "ExternalTimer"
SELECT ExternalTimer.platformId
     , ExternalTimer.variantId
     , ExternalTimer.implId
     , ExternalTimer.name
     , ExternalTimer.minTime
     , ExternalTimer.avgTime
     , ExternalTimer.maxTime
     , ExternalTimer.stdDev
     , ExternalImpl.algorithmId
FROM ExternalTimer

INNER JOIN ExternalImpl
ON ExternalTimer.implId = ExternalImpl.id
|]
    ]
