{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Variant where

import Data.String.Interpolate.IsString (i)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH
import Database.Persist.Types

import Pretty.Columns
import Schema.Utils (EntityDef, Int64, MonadSql, (.>), (.=))
import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import Schema.Graph (GraphId)
import Schema.VariantConfig (VariantConfigId)
import qualified Schema.Variant.V0 as V0
import qualified Schema.Variant.V1 as V1

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema'"] [persistUpperCase|
Variant
    graphId GraphId
    variantConfigId VariantConfigId
    algorithmId AlgorithmId
    result Hash Maybe
    propsStored Bool
    retryCount Int
    UniqVariant graphId variantConfigId
    deriving Eq Show
|]

instance PrettyColumns Variant where
    prettyColumnInfo = idColumn VariantId :|
        [ idColumn VariantVariantConfigId
        , idColumn VariantAlgorithmId
        , idColumn VariantGraphId
        , VariantPropsStored `columnVia` prettyShow
        , VariantRetryCount `columnVia` prettyShow
        , VariantResult `maybeColumnVia` prettyShow
        ]

schema :: [EntityDef]
schema = Utils.addForeignRef "Variant" variantConfig schema'
  where
    variantConfig :: ForeignDef
    variantConfig = Utils.mkForeignRef "VariantConfig"
        [ ("variantConfigId", "id"), ("algorithmId", "algorithmId") ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .= V0.schema
    , 7 .= V1.schema
    , 9 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "Variant" ADD COLUMN "algorithmId" INTEGERS REFERENCES "Algorithm"
|]

        Utils.executeSql [i|
REPLACE INTO "Variant"
SELECT Variant.id
     , Variant.graphId
     , Variant.variantConfigId
     , Variant.result
     , Variant.propsStored
     , Variant.retryCount
     , VariantConfig.algorithmId
FROM Variant

INNER JOIN VariantConfig
ON Variant.variantConfigId = VariantConfig.id
|]
    ]
