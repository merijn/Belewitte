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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.RunConfig where

import Data.String.Interpolate.IsString (i)
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
    , (.=)
    )
import qualified Schema.Utils as Utils
import Types

-- Schema version for RunConfig table initialisation
import qualified Schema.Platform.V1 as Platform.V1

-- Schema version for current schema
import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Dataset (DatasetId)
import qualified Schema.Dataset as Dataset
import Schema.Platform (PlatformId)
import qualified Schema.Platform as Platform
import qualified Schema.RunConfig.V0 as V0

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Dataset.schema, Platform.schema] [Utils.mkSchema|
RunConfig
    algorithmId AlgorithmId
    platformId PlatformId
    datasetId DatasetId
    algorithmVersion CommitId
    repeats Int
    UniqRunConfig algorithmId platformId datasetId algorithmVersion
    deriving Eq Show
|]

deriving instance Show (Unique RunConfig)

instance PrettyFields (Entity RunConfig) where
    prettyFieldInfo = ("Id", idField RunConfigId) :|
        [ ("Algorithm", namedIdField RunConfigAlgorithmId)
        , ("Platform", namedIdField RunConfigPlatformId)
        , ("Dataset", namedIdField RunConfigDatasetId)
        , ("Repeats", RunConfigRepeats `fieldVia` prettyShow)
        , ("Algorithm Commit", RunConfigAlgorithmVersion `fieldVia` getCommitId)
        ]

migrations
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 6 .> V0.schema $ do
        Utils.createTableFromSchema
            [ Algorithm.schema
            , Dataset.schema
            , Platform.V1.schema
            , schema
            ]

        Utils.executeSql [i|
INSERT INTO "RunConfig"
SELECT ROW_NUMBER() OVER (ORDER BY algorithmId, platformId, datasetId)
     , algorithmId
     , platformId
     , datasetId
     , "Unknown"
     , 0
FROM (
    SELECT DISTINCT Variant.algorithmId, TotalTimer.platformId, Graph.datasetId
    FROM TotalTimer

    INNER JOIN Variant
    ON Variant.id = TotalTimer.variantId

    INNER JOIN Graph
    ON Graph.id = Variant.graphId
)
|]
    , 16 .= schema
    ]
