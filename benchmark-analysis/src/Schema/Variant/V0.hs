{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Variant.V0 where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Types

import Schema.Algorithm hiding (schema)
import Schema.Graph hiding (schema)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Variant
    graphId GraphId
    algorithmId AlgorithmId
    name Text
    flags Text Maybe
    result Hash Maybe
    propsStored Bool
    retryCount Int
    UniqVariant graphId algorithmId name
    deriving Eq Show
|]
