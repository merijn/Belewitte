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
module Schema.RunConfig where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Columns
import Schema.Utils (EntityDef, Int64, MonadSql, (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Dataset (DatasetId)
import Schema.Platform (PlatformId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
RunConfig
    algorithmId AlgorithmId
    platformId PlatformId
    datasetId DatasetId
    algorithmVersion Text
    repeats Int
    deriving Eq Show
|]

instance PrettyColumns RunConfig where
    prettyColumnInfo = idColumn RunConfigId :|
        [ idColumn RunConfigAlgorithmId
        , idColumn RunConfigPlatformId
        , idColumn RunConfigDatasetId
        , RunConfigRepeats `columnVia` prettyShow
        , column RunConfigAlgorithmVersion
        ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 6 .> schema $ do
        Utils.createTableFromSchema schema

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
    ]
