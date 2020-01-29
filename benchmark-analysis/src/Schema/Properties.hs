{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Properties where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Fields
import Schema.Utils
    (EntityDef, ForeignDef, Int64, MonadSql, Transaction, (.=), (.>))
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Graph (GraphId)
import Schema.Variant (VariantId)
import qualified Schema.Properties.V0 as V0

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema'"] [persistUpperCase|
GraphProp
    graphId GraphId
    property Text
    value Double
    Primary graphId property
    UniqGraphProp graphId property
    deriving Eq Show

StepProp
    algorithmId AlgorithmId
    property Text
    Primary algorithmId property
    UniqStepProp algorithmId property
    deriving Eq Show

StepPropValue
    algorithmId AlgorithmId
    variantId VariantId
    stepId Int
    property Text
    value Double
    Primary variantId stepId property
    UniqStepPropValue variantId stepId property
    deriving Eq Show
|]

schema :: [EntityDef]
schema = Utils.addForeignRef "StepPropValue" stepProp
       . Utils.addForeignRef "StepPropValue" variant
       $ schema'
  where
    stepProp :: ForeignDef
    stepProp = Utils.mkForeignRef "StepProp"
        [ ("algorithmId", "algorithmId"), ("property", "property") ]

    variant :: ForeignDef
    variant = Utils.mkForeignRef "Variant"
        [ ("variantId", "id"), ("algorithmId", "algorithmId") ]

instance PrettyFields GraphProp where
    prettyFieldInfo = ("Graph", idField GraphPropGraphId) :|
        [ ("Property", textField GraphPropProperty)
        , ("Value", GraphPropValue `fieldVia` prettyDouble)
        ]

instance PrettyFields StepPropValue where
    prettyFieldInfo = ("Algorithm", idField StepPropValueAlgorithmId) :|
        [ ("Variant", idField StepPropValueVariantId)
        , ("Step", StepPropValueStepId `fieldVia` prettyShow)
        , ("Property", textField StepPropValueProperty)
        , ("Value", StepPropValueValue `fieldVia` prettyDouble)
        ]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .= V0.schema
    , 14 .> schema $ do

        Utils.executeSql [i|
ALTER TABLE "StepProp" RENAME TO "StepPropValue"
|]

        Utils.executeSql [i|
ALTER TABLE "StepPropValue"
ADD COLUMN "algorithmId" INTEGER
|]

        Utils.executeSql [i|
REPLACE INTO  "StepPropValue"
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
    ]
