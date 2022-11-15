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
module Schema.UnknownPredictions where

import Control.Monad (when)
import Control.Monad.Logger (logErrorN)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate.IsString (i)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)

import Exceptions (AbortMigration(..), logThrowM)
import Schema.Utils
    ( EntityDef
    , ForeignDef
    , Int64
    , MonadLogger
    , MonadSql
    , MonadThrow
    , Transaction
    , (.>)
    )
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Model (PredictionModelId)
import Schema.Implementation (ImplementationId)
import qualified Schema.UnknownPredictions.V0 as V0
import qualified Schema.UnknownPredictions.V1 as V1
import qualified Schema.UnknownPredictions.V2 as V2

Utils.mkEntities "schema'" [persistUpperCase|
UnknownPrediction
    modelId PredictionModelId
    algorithmId AlgorithmId
    unknownSetId Int64
    count Int
    UniqUnknownPrediction modelId unknownSetId
    deriving Eq Show

UnknownPredictionSet
    unknownPredId UnknownPredictionId
    implId ImplementationId
    algorithmId AlgorithmId
    Primary unknownPredId implId
    deriving Eq Show
|]

deriving instance Show (Unique UnknownPrediction)

schema :: [EntityDef]
schema = Utils.addForeignRef "UnknownPrediction" model
       . Utils.addForeignRef "UnknownPredictionSet" unknownPred
       $ schema'
  where
    model :: ForeignDef
    model = Utils.mkForeignRef "PredictionModel"
        [ ("modelId", "id"), ("algorithmId", "algorithmId") ]

    unknownPred :: ForeignDef
    unknownPred = Utils.mkForeignRef "UnknownPrediction"
        [ ("unknownPredId", "id"), ("algorithmId", "algorithmId") ]

reconcileTables
    :: (MonadLogger m, MonadSql m, MonadThrow m) => Transaction m ()
reconcileTables = do
    n <- Utils.executeSqlSingleValue [i|
WITH
    SetUnion AS (
        SELECT * FROM ModelUnknown
        UNION
        SELECT * FROM UnknownPrediction
    ),
    SetIntersect AS (
        SELECT * FROM UnknownPrediction
        INTERSECT
        SELECT * FROM ModelUnknown
    )
SELECT COUNT(*)
FROM (
    SELECT * FROM SetUnion
    EXCEPT
    SELECT * FROM SetIntersect
)|]

    when (n > (0 :: Int)) $ do
        logErrorN "Conflicting data found in \"ModelUnknown\" and\
                  \ \"UnknownPrediction\" tables!"
        logThrowM AbortMigration

    Utils.executeSql [i|DROP TABLE 'ModelUnknown'|]

updateModelUnknownTable
    :: (MonadLogger m, MonadSql m, MonadThrow m) => Transaction m ()
updateModelUnknownTable = do
    Utils.executeSql [i|
ALTER TABLE "ModelUnknown"
RENAME COLUMN "unknownCount" TO "count"
|]

    Utils.executeSql [i|
ALTER TABLE "ModelUnknown"
RENAME TO "UnknownPrediction"
|]

    Utils.executeSql [i|
ALTER TABLE "UnknownSet"
RENAME COLUMN "modelUnknownId" TO "unknownPredId"
|]

migrations
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .> V0.schema $ do
        modelUnknownExists <- fromMaybe False <$>
            Utils.executeSqlSingleValueMaybe [i|
SELECT 1
FROM "sqlite_master"
WHERE type = 'table' AND name = 'ModelUnknown'
|]

        unknownPredExists <- fromMaybe False <$>
            Utils.executeSqlSingleValueMaybe [i|
SELECT 1
FROM "sqlite_master"
WHERE type = 'table' AND name = 'UnknownPrediction'
|]

        when modelUnknownExists $ do
            if unknownPredExists
               then reconcileTables
               else updateModelUnknownTable

    , 9 .> V1.schema $ do
        Utils.executeSql [i|
ALTER TABLE "UnknownPrediction"
ADD COLUMN "algorithmId" INTEGER REFERENCES "Algorithm"
|]

        Utils.executeSql [i|
ALTER TABLE "UnknownSet"
ADD COLUMN "algorithmId" INTEGER REFERENCES "Algorithm"
|]

        Utils.executeSql [i|
REPLACE INTO "UnknownSet"
SELECT UnknownSet.unknownPredId
     , UnknownSet.implId
     , AlgorithmMapping.algorithmId
FROM UnknownSet

INNER JOIN
(   SELECT UnknownSet.unknownPredId, Implementation.algorithmId
    FROM UnknownSet

    INNER JOIN Implementation
    ON UnknownSet.implId = Implementation.id

    GROUP BY UnknownSet.unknownPredId, Implementation.algorithmId
) AS AlgorithmMapping
ON UnknownSet.unknownPredId = AlgorithmMapping.unknownPredId
|]

        Utils.executeSql [i|
REPLACE INTO "UnknownPrediction"
SELECT UnknownPrediction.id
     , UnknownPrediction.modelId
     , UnknownPrediction.count
     , AlgorithmMapping.algorithmId
FROM UnknownPrediction

INNER JOIN
(   SELECT UnknownSet.unknownPredId, Implementation.algorithmId
    FROM UnknownSet

    INNER JOIN Implementation
    ON UnknownSet.implId = Implementation.id

    GROUP BY UnknownSet.unknownPredId, Implementation.algorithmId
) AS AlgorithmMapping
ON UnknownPrediction.id = AlgorithmMapping.unknownPredId
|]
    , 12 .> V2.schema $ do

        Utils.executeSql [i|
ALTER TABLE "UnknownSet" RENAME TO "UnknownPredictionSet"
|]
    , 13 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "UnknownPrediction"
ADD COLUMN "unknownSetId" INTEGER
|]

        Utils.executeSql [i|
REPLACE INTO "UnknownPrediction"
SELECT UnknownPrediction.id
     , UnknownPrediction.modelId
     , UnknownPrediction.algorithmId
     , UnknownPrediction.count
     , 1 + ROW_NUMBER() OVER (PARTITION BY modelId ORDER BY count)
FROM UnknownPrediction
|]
    ]
