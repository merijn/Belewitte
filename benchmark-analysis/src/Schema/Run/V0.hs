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

import qualified Schema.Utils as Utils

import Schema.Implementation (ImplementationId)
import qualified Schema.Implementation as Implementation
import Schema.RunConfig (RunConfigId)
import qualified Schema.RunConfig as RunConfig
import Schema.Variant (VariantId)
import qualified Schema.Variant as Variant

Utils.mkEntitiesWith "schema"
    [Implementation.schema, RunConfig.schema, Variant.schema] [Utils.mkSchema|
Run
    runConfigId RunConfigId
    variantId VariantId
    implId ImplementationId
    timestamp UTCTime
    validated Bool
    UniqRun runConfigId variantId implId
    deriving Eq Show
|]
