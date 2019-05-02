{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.ByteString (ByteString)
import Database.Persist.Class (PersistField(..))
import Database.Persist.TH
import Database.Persist.Sql (PersistFieldSql(..))

data ImplType = Builtin | Core | Derived | Comparison
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
derivePersistField "ImplType"

newtype Hash = Hash { getHash :: ByteString }
    deriving (Show, Read, Eq, PersistField, PersistFieldSql)
