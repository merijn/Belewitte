{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Properties where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Columns
import Schema.Utils (EntityDef, Int64, MonadSql, (.=))
import qualified Schema.Utils as Utils

import Schema.Graph (GraphId)
import Schema.Variant (VariantId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
GraphProp
    graphId GraphId
    property Text
    value Double
    Primary graphId property
    UniqGraphProp graphId property
    deriving Eq Show

StepProp
    variantId VariantId
    stepId Int
    property Text
    value Double
    Primary variantId stepId property
    UniqStepProp variantId stepId property
    deriving Eq Show
|]

instance PrettyColumns GraphProp where
    prettyColumnInfo = ("Graph", idColumn GraphPropGraphId) :|
        [ ("Property", column GraphPropProperty)
        , ("Value", GraphPropValue `columnVia` prettyDouble)
        ]

instance PrettyColumns StepProp where
    prettyColumnInfo = ("Variant", idColumn StepPropVariantId) :|
        [ ("Step", StepPropStepId `columnVia` prettyShow)
        , ("Property", column StepPropProperty)
        , ("Value", StepPropValue `columnVia` prettyDouble)
        ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup [ 0 .= schema ]
