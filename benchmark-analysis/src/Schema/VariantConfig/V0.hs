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
module Schema.VariantConfig.V0 where

import Data.Text (Text)

import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm

Utils.mkEntitiesWith "schema" [Algorithm.schema] [Utils.mkSchema|
VariantConfig
    algorithmId AlgorithmId
    name Text
    flags Text Maybe
    isDefault Bool
    UniqVariantConfig algorithmId name
    deriving Eq Show
|]
