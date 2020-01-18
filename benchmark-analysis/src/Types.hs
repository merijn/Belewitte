{-# OPTIONS_GHC -fno-prof-auto #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Crypto.Hash (Digest, MD5, digestFromByteString)
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Database.Persist.Class (PersistField(..))
import Database.Persist.TH
import Database.Persist.Sql (PersistFieldSql(..))

data ImplType = Builtin | Core | Derived
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
derivePersistField "ImplType"

type HashDigest = Digest MD5

newtype Hash = Hash { getHash :: ByteString }
    deriving (Read, Eq, PersistField, PersistFieldSql)

instance Show Hash where
    show (Hash bs) = show . fromJust $
        (digestFromByteString bs :: Maybe HashDigest)

newtype CommitId = CommitId { getCommitId :: Text }
    deriving (Read, Eq, PersistField, PersistFieldSql)

instance Show CommitId where
    show (CommitId hash) = unpack hash
