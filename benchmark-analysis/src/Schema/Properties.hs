{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema.Properties where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Utils (Int64, MigrationAction, mkMigrationLookup)

import Schema.Graph (GraphId)
import Schema.Variant (VariantId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
GraphProp
    graphId GraphId
    property Text
    value Double
    Primary graphId property
    deriving Eq Show

StepProp
    variantId VariantId
    stepId Int
    property Text
    value Double
    Primary variantId stepId property
    deriving Eq Show
|]

migrations :: Int64 -> MigrationAction
migrations = mkMigrationLookup schema []
