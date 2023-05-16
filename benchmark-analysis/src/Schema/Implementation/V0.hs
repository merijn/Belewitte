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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Implementation.V0 where

import Data.Text (Text)

import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm

Utils.mkEntitiesWith "schema" [Algorithm.schema] [Utils.mkSchema|
Implementation
    algorithmId AlgorithmId
    name Text
    prettyName Text Maybe
    flags Text Maybe
    type ImplType
    UniqImpl algorithmId name
    deriving Eq Show
|]
