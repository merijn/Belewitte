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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.External where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Database.Persist.Sql (Unique)

import Pretty.Fields.Persistent
import Schema.Import
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

-- Schema versions for ExternalImpl/ExternalTimer table initialisations
import qualified Schema.Graph.V1 as Graph.V1
import qualified Schema.Platform.V1 as Platform.V1
import qualified Schema.Variant.V0 as Variant.V0

-- Schema versions for current schema
import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Platform (PlatformId)
import qualified Schema.Platform as Platform
import Schema.Variant (VariantId)
import qualified Schema.Variant as Variant
import qualified Schema.External.V0 as V0

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Platform.schema, Variant.schema] [Utils.mkSchema|
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
    Foreign Variant foreignVariant variantId algorithmId References Id algorithmId
    Foreign ExternalImpl foreignExternalImpl implId algorithmId References Id algorithmId
    deriving Eq Show
|]

deriving instance Show (Unique ExternalImpl)

instance PrettyFields (Entity ExternalImpl) where
    prettyFieldInfo = ("Id", idField ExternalImplId) :|
        [ ("Algorithm", namedIdField ExternalImplAlgorithmId)
        , ("Name", textField ExternalImplName)
        , ("Pretty Name", maybeTextField ExternalImplPrettyName)
        ]

instance PrettyFields (Entity ExternalTimer) where
    prettyFieldInfo = ("Platform", namedIdField ExternalTimerPlatformId) :|
        [ ("Variant", idField ExternalTimerVariantId)
        , ("Algorithm", namedIdField ExternalTimerAlgorithmId)
        , ("Implementation", namedIdField ExternalTimerImplId)
        , ("Name", textField ExternalTimerName)
        , ("Min. Time", doubleField 0 ExternalTimerMinTime)
        , ("Avg. Time", doubleField 0 ExternalTimerAvgTime)
        , ("Max Time", doubleField 0 ExternalTimerMaxTime)
        , ("Std. Dev.", doubleField_ ExternalTimerStdDev)
        ]

instance NamedEntity ExternalImpl where
    entityName = optionalPrettyName externalImplPrettyName externalImplName

instance Importable ExternalImpl where
    updateFields = [ForeignKeyField ExternalImplAlgorithmId]

instance Importable ExternalTimer where
    importType _ = PrimaryImport
    updateFields =
        [ ForeignKeyField ExternalTimerPlatformId
        , ForeignKeyField ExternalTimerVariantId
        , ForeignKeyField ExternalTimerImplId
        , ForeignKeyField ExternalTimerAlgorithmId
        ]

migrations
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 4 .> V0.schema $ do
        Utils.createTableFromSchema
            [ Algorithm.schema
            , Graph.V1.schema
            , Platform.V1.schema
            , Variant.V0.schema
            , V0.schema
            ]

        Utils.executeSql [i|
ALTER TABLE "ExternalImpl"
ADD COLUMN "oldImplId" INTEGER REFERENCES "Implementation"
|]

        Utils.executeSql [i|
INSERT INTO "ExternalImpl"
SELECT ROW_NUMBER() OVER (ORDER BY id), algorithmId, name, prettyName, id
FROM Implementation WHERE type = "Comparison"
|]

        Utils.executeSql [i|
INSERT INTO "ExternalTimer"
SELECT TotalTimer.platformId, TotalTimer.variantId, ExternalImpl.id
     , TotalTimer.name, TotalTimer.minTime, TotalTimer.avgTime
     , TotalTimer.maxTime, TotalTimer.stdDev
FROM TotalTimer
INNER JOIN ExternalImpl ON TotalTimer.implId = ExternalImpl.oldImplId
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
