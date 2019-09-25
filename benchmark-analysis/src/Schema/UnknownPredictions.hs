{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.UnknownPredictions where

import Data.String.Interpolate.IsString (i)
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Utils (EntityDef, ForeignDef, Int64, MonadSql, (.=), (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Model (PredictionModelId)
import Schema.Implementation (ImplementationId)
import qualified Schema.UnknownPredictions.V0 as V0

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema'"] [persistUpperCase|
UnknownPrediction
    modelId PredictionModelId
    algorithmId AlgorithmId
    count Int
    deriving Eq Show

UnknownSet
    unknownPredId UnknownPredictionId
    implId ImplementationId
    algorithmId AlgorithmId
    Primary unknownPredId implId
    deriving Eq Show
|]

schema :: [EntityDef]
schema = Utils.addForeignRef "UnknownPrediction" model
       . Utils.addForeignRef "UnknownSet" unknownPred
       $ schema'
  where
    model :: ForeignDef
    model = Utils.mkForeignRef "PredictionModel"
        [ ("modelId", "id"), ("algorithmId", "algorithmId") ]

    unknownPred :: ForeignDef
    unknownPred = Utils.mkForeignRef "UnknownPrediction"
        [ ("unknownPredId", "id"), ("algorithmId", "algorithmId") ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .= V0.schema
    , 9 .> schema $ do
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
    ]
