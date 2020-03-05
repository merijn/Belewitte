{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Properties.V0 where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

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
