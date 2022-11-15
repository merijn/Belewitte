{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Properties where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)

import Pretty.Fields.Persistent
import Schema.Utils
    (Entity, EntityDef, ForeignDef, Int64, MonadSql, Transaction, (.=), (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Graph (GraphId)
import Schema.Variant (VariantId)
import qualified Schema.Properties.V0 as V0
import qualified Schema.Properties.V1 as V1

Utils.mkEntities "schema'" [persistUpperCase|
PropertyName
    property Text
    isStepProp Bool
    UniqProperty property
    deriving Eq Show

GraphPropValue
    graphId GraphId
    propId PropertyNameId
    value Double
    Primary graphId propId
    UniqGraphPropValue graphId propId
    deriving Eq Show

StepProp
    propId PropertyNameId
    algorithmId AlgorithmId
    Primary propId algorithmId
    UniqStepProp propId algorithmId
    deriving Eq Show

StepPropValue
    variantId VariantId
    stepId Int
    propId PropertyNameId
    algorithmId AlgorithmId
    value Double
    Primary variantId stepId propId
    UniqStepPropValue variantId stepId propId
    deriving Eq Show
|]

deriving instance Show (Unique PropertyName)
deriving instance Show (Unique GraphPropValue)
deriving instance Show (Unique StepProp)
deriving instance Show (Unique StepPropValue)

schema :: [EntityDef]
schema = Utils.addForeignRef "StepPropValue" stepProp
       . Utils.addForeignRef "StepPropValue" variant
       $ schema'
  where
    stepProp :: ForeignDef
    stepProp = Utils.mkForeignRef "StepProp"
        [ ("algorithmId", "algorithmId"), ("propId", "propId") ]

    variant :: ForeignDef
    variant = Utils.mkForeignRef "Variant"
        [ ("variantId", "id"), ("algorithmId", "algorithmId") ]

pattern GraphPropName :: Text -> PropertyName
pattern GraphPropName name = PropertyName name False
pattern StepPropName :: Text -> PropertyName
pattern StepPropName name = PropertyName name True

instance PrettyFields (Entity PropertyName) where
    prettyFieldInfo = ("Id", idField PropertyNameId) :|
        [ ("Property", textField PropertyNameProperty)
        ]

instance NamedEntity PropertyName where
    entityName = propertyNameProperty

instance PrettyFields (Entity GraphPropValue) where
    prettyFieldInfo = ("Graph", idField GraphPropValueGraphId) :|
        [ ("Property", namedIdField GraphPropValuePropId)
        , ("Value", doubleField_ GraphPropValueValue)
        ]

instance PrettyFields (Entity StepProp) where
    prettyFieldInfo = ("Property", idField StepPropPropId) :|
        [ ("Algorithm", namedIdField StepPropAlgorithmId)
        ]

instance PrettyFields (Entity StepPropValue) where
    prettyFieldInfo = ("Algorithm", namedIdField StepPropValueAlgorithmId) :|
        [ ("Variant", idField StepPropValueVariantId)
        , ("Step", StepPropValueStepId `fieldVia` prettyShow)
        , ("Property", namedIdField StepPropValuePropId)
        , ("Value", doubleField_ StepPropValueValue)
        ]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .= V0.schema
    , 14 .> V1.schema $ do

        Utils.executeSql [i|
ALTER TABLE "StepProp" RENAME TO "StepPropValue"
|]

        Utils.executeSql [i|
ALTER TABLE "StepPropValue"
ADD COLUMN "algorithmId" INTEGER
|]

        Utils.executeSql [i|
REPLACE INTO "StepPropValue"
SELECT StepPropValue.variantId
     , StepPropValue.stepId
     , StepPropValue.property
     , StepPropValue.value
     , Variant.algorithmId
FROM StepPropValue
INNER JOIN Variant
ON StepPropValue.variantId = Variant.id
|]

        Utils.executeSql [i|
CREATE TABLE "StepProp" AS
SELECT DISTINCT algorithmId, property
FROM StepPropValue
|]
    , 24 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "GraphProp" RENAME TO "GraphPropValue"
|]

        Utils.executeSql [i|
CREATE TABLE "PropertyName"
("id" INTEGER, "property" TEXT UNIQUE, "isStepProp" BOOLEAN)
|]

        Utils.executeSql [i|
INSERT INTO "PropertyName"
SELECT ROW_NUMBER() OVER props AS id, property, 0
FROM (SELECT DISTINCT property FROM GraphPropValue)
WINDOW props AS (ORDER BY property)
|]

        Utils.executeSql [i|
INSERT INTO "PropertyName"
SELECT (SELECT MAX(id) FROM PropertyName) + ROW_NUMBER() OVER props
     , property
     , 1
FROM (SELECT DISTINCT property FROM StepProp)
WINDOW props AS (ORDER BY property)
|]

        Utils.executeSql [i|
CREATE TABLE "ModelProperty"
("modelId" INTEGER, "propId" INTEGER, "importance" REAL)
|]

        Utils.executeSql [i|
INSERT INTO "ModelProperty"
SELECT modelId, PropertyName.id, importance
FROM ModelGraphProperty
INNER JOIN PropertyName
ON PropertyName.property = ModelGraphProperty.property
|]

        Utils.executeSql [i|
INSERT INTO "ModelProperty"
SELECT modelId, PropertyName.id, importance
FROM ModelStepProperty
INNER JOIN PropertyName
ON PropertyName.property = ModelStepProperty.property
|]

        Utils.executeSql [i|
ALTER TABLE "GraphPropValue"
ADD COLUMN "propId" INTEGER
|]

        Utils.executeSql [i|
REPLACE INTO "GraphPropValue"
SELECT GraphPropValue.graphId
     , GraphPropValue.property
     , GraphPropValue.value
     , PropertyName.id
FROM GraphPropValue
INNER JOIN PropertyName
ON GraphPropValue.property = PropertyName.property
AND NOT PropertyName.isStepProp
|]

        Utils.executeSql [i|
ALTER TABLE "StepProp"
ADD COLUMN "propId" INTEGER
|]

        Utils.executeSql [i|
REPLACE INTO "StepProp"
SELECT StepProp.algorithmId
     , StepProp.property
     , PropertyName.id
FROM StepProp
INNER JOIN PropertyName
ON StepProp.property = PropertyName.property
AND PropertyName.isStepProp
|]

        Utils.executeSql [i|
ALTER TABLE "StepPropValue"
ADD COLUMN "propId" INTEGER
|]

        Utils.executeSql [i|
REPLACE INTO "StepPropValue"
SELECT StepPropValue.algorithmId
     , StepPropValue.variantId
     , StepPropValue.stepId
     , StepPropValue.property
     , StepPropValue.value
     , PropertyName.id
FROM StepPropValue
INNER JOIN StepProp
ON StepPropValue.property = StepProp.property
AND StepPropValue.algorithmId = StepProp.algorithmId
INNER JOIN PropertyName
ON StepProp.propId = PropertyName.id
AND PropertyName.isStepProp
|]
    ]
