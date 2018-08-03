{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.ByteString (ByteString)
import Database.Persist.Class (PersistField(..))
import Database.Persist.TH
import Database.Persist.Sql (PersistFieldSql(..))

data ImplType = Core | Derived | Comparison
    deriving (Show, Read, Eq, Ord)
derivePersistField "ImplType"

newtype Hash = Hash { getHash :: ByteString }
    deriving (Show, Read, Eq, PersistField, PersistFieldSql)
