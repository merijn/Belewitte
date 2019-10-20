{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.VariantConfig where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Fields
import Schema.Utils (EntityDef, Int64, MonadSql, (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
VariantConfig
    algorithmId AlgorithmId
    name Text
    flags Text Maybe
    isDefault Bool
    UniqVariantConfig algorithmId name
    deriving Eq Show
|]

instance PrettyFields VariantConfig where
    prettyFieldInfo = ("Id", idField VariantConfigId) :|
        [ ("Name", textField VariantConfigName)
        , ("Algorithm", idField VariantConfigAlgorithmId)
        , ("Flags", maybeTextField VariantConfigFlags)
        , ( "Default Variant Config"
          , VariantConfigIsDefault `fieldVia` prettyShow
          )
        ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 7 .> schema $ do
        Utils.createTableFromSchema schema

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
    ]
