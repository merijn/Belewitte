{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.VariantConfig where

import Control.Monad (when)
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (Checkmark, Unique)
import Database.Persist.TH (persistUpperCase)

import Exceptions (DBConstraintViolation(..), logThrowM)
import Pretty.Fields.Persistent
import Schema.Import
import Schema.Utils
    (Entity, EntityDef, Int64, MonadLogger, MonadSql, MonadThrow, Transaction, (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import qualified Schema.VariantConfig.V0 as V0
import qualified Schema.VariantConfig.V1 as V1

Utils.mkEntities "schema" [persistUpperCase|
VariantConfig
    algorithmId AlgorithmId
    name Text
    flags Text Maybe
    isDefault Checkmark nullable
    timestamp UTCTime
    UniqVariantConfig algorithmId flags !force
    UniqVariantConfigName algorithmId name
    UniqVariantConfigDefault algorithmId isDefault !force
    deriving Eq Show
|]

deriving instance Show (Unique VariantConfig)

instance PrettyFields (Entity VariantConfig) where
    prettyFieldInfo = ("Id", idField VariantConfigId) :|
        [ ("Name", textField VariantConfigName)
        , ("Algorithm", namedIdField VariantConfigAlgorithmId)
        , ("Flags", maybeTextField VariantConfigFlags)
        , ( "Default Variant Config"
          , VariantConfigIsDefault `fieldVia` prettyShow
          )
        ]

instance NamedEntity VariantConfig where
    entityName = variantConfigName

instance Importable VariantConfig where
    importType _ = ExplicitUniqueImport $ \case
        UniqVariantConfig{} -> True
        _ -> False

    updateFields = [ForeignKeyField VariantConfigAlgorithmId]

migrations
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Int64 -> Transaction m [EntityDef]
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
    , 20 .> V1.schema $ do
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
    , 29 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "VariantConfig"
RENAME COLUMN "isDefault" TO "oldIsDefault"
|]

        Utils.executeSql [i|
ALTER TABLE "VariantConfig"
ADD COLUMN "isDefault" BOOLEAN
|]

        Utils.executeSql [i|
UPDATE "VariantConfig"
SET "isDefault" =
  CASE WHEN "oldIsDefault" THEN 1
    ELSE NULL
  END
|]

        violated <- (/= (0 :: Int)) <$> Utils.executeSqlSingleValue [i|
SELECT COUNT(*) FROM (
    SELECT "algorithmId", "flags", COUNT(*) AS "count"
    FROM "VariantConfig"
    GROUP BY "algorithmId", "flags"
    HAVING "count" > 1
)
|]
        when violated $ do
            logThrowM $ DBConstraintViolation "Duplicate variant configs!"
    ]
