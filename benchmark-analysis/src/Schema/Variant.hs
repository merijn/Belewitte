{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema.Variant where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Utils (Int64, MigrationAction, mkMigrationLookup)
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

migrations :: Int64 -> MigrationAction
migrations = mkMigrationLookup schema []
