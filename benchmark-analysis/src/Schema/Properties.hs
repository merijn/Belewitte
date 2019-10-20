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

import Pretty.Fields
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

instance PrettyFields GraphProp where
    prettyFieldInfo = ("Graph", idField GraphPropGraphId) :|
        [ ("Property", textField GraphPropProperty)
        , ("Value", GraphPropValue `fieldVia` prettyDouble)
        ]

instance PrettyFields StepProp where
    prettyFieldInfo = ("Variant", idField StepPropVariantId) :|
        [ ("Step", StepPropStepId `fieldVia` prettyShow)
        , ("Property", textField StepPropProperty)
        , ("Value", StepPropValue `fieldVia` prettyDouble)
        ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup [ 0 .= schema ]
