{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE UndecidableInstances #-}
module Schema.VariantConfig where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Fields
import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import qualified Schema.VariantConfig.V0 as V0 

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
VariantConfig
    algorithmId AlgorithmId
    name Text
    flags Text Maybe
    isDefault Bool
    timestamp UTCTime
    UniqVariantConfig algorithmId name
    deriving Eq Show
|]

deriving instance Show (Unique VariantConfig)

instance PrettyFields VariantConfig where
    prettyFieldInfo = ("Id", idField VariantConfigId) :|
        [ ("Name", textField VariantConfigName)
        , ("Algorithm", idField VariantConfigAlgorithmId)
        , ("Flags", maybeTextField VariantConfigFlags)
        , ( "Default Variant Config"
          , VariantConfigIsDefault `fieldVia` prettyShow
          )
        ]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 7 .> V0.schema $ do
        Utils.createTableFromSchema V0.schema

        Utils.executeSql [i|
INSERT INTO "VariantConfig"
SELECT ROW_NUMBER() OVER (ORDER BY name)
     , algorithmId
     , name
     , flags
     , FALSE
FROM (
    SELECT DISTINCT Variant.algorithmId, Variant.name, Variant.flags
    FROM Variant
)
|]

        Utils.executeSql [i|
ALTER TABLE "Variant"
ADD COLUMN "variantConfigId" INTEGER REFERENCES "VariantConfig"
|]

        Utils.executeSql [i|
REPLACE INTO Variant
SELECT Variant.id
     , Variant.graphId
     , Variant.algorithmId
     , Variant.name
     , Variant.flags
     , Variant.result
     , Variant.propsStored
     , Variant.retryCount
     , VariantConfig.id
FROM Variant

INNER JOIN VariantConfig
ON  Variant.algorithmId = VariantConfig.algorithmId
AND Variant.name = VariantConfig.name
AND Variant.flags IS VariantConfig.flags
|]
    , 20 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "VariantConfig"
ADD COLUMN "timestamp" TIMESTAMP
|]

        Utils.executeSql [i|
REPLACE INTO "VariantConfig"
SELECT VariantConfig.id
     , VariantConfig.algorithmId
     , VariantConfig.name
     , VariantConfig.flags
     , VariantConfig.isDefault
     , IFNULL(TimeStamp.minTime, strftime('%Y-%m-%dT%H:%M:%f', 'now'))
FROM VariantConfig

LEFT JOIN
(   SELECT VariantConfig.id, MIN(Run.timestamp) AS minTime
    FROM VariantConfig

    INNER JOIN Variant
    ON VariantConfig.id = Variant.variantConfigId

    INNER JOIN Run
    ON Variant.id = Run.variantId

    GROUP BY VariantConfig.id
) AS TimeStamp
ON VariantConfig.id = TimeStamp.id
|]
    ]
