{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema.Algorithm where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Utils (Int64, MigrationAction, mkMigrationLookup)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Algorithm
    name Text
    prettyName Text Maybe
    UniqAlgorithm name
    deriving Eq Show
|]

migrations :: Int64 -> MigrationAction
migrations = mkMigrationLookup schema []
