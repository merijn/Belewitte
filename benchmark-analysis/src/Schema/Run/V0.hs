{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Run.V0 where

import Data.Time.Clock (UTCTime)
import Database.Persist.TH (persistUpperCase)

import qualified Schema.Utils as Utils

import Schema.Implementation (ImplementationId)
import Schema.RunConfig (RunConfigId)
import Schema.Variant (VariantId)

Utils.mkEntities "schema" [persistUpperCase|
Run
    runConfigId RunConfigId
    variantId VariantId
    implId ImplementationId
    timestamp UTCTime
    validated Bool
    UniqRun runConfigId variantId implId
    deriving Eq Show
|]
