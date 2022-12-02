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
module Schema.Variant.V0 where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)

import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Graph (GraphId)
import qualified Schema.Graph as Graph

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Graph.schema] [persistUpperCase|
Variant
    graphId GraphId
    algorithmId AlgorithmId
    name Text
    flags Text Maybe
    result Hash Maybe
    propsStored Bool default=0
    retryCount Int default=0
    UniqVariant graphId algorithmId name
    deriving Eq Show
|]
