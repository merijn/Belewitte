{-# OPTIONS_GHC -fno-prof-auto #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
    ( CommitId(..)
    , HashDigest
    , Hash(..)
    , ImplType(..)
    , Percentage
    , getPercentage
    , mkPercentage
    ) where

import Crypto.Hash (Digest, MD5, digestFromByteString)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(Proxy))
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
    deriving (Read, Eq, Ord, PersistField, PersistFieldSql)

instance Show CommitId where
    show (CommitId hash) = unpack hash

newtype Percentage = Percentage { getPercentage :: Double}
    deriving (Read, Eq, Ord)

mkPercentage :: Double -> Maybe Percentage
mkPercentage d
    | d >= 0 && d <= 1 = Just $ Percentage d
    | otherwise = Nothing

instance PersistField Percentage where
    toPersistValue = toPersistValue . getPercentage
    fromPersistValue val = fromPersistValue val >>= toPercentage
      where
        toPercentage :: Double -> Either Text Percentage
        toPercentage d
            | d < 0 = Left "Percentages can't be smaller than 0"
            | d > 1 = Left "Percentages can't be larger than 1"
            | otherwise = Right $ Percentage d

instance PersistFieldSql Percentage where
    sqlType _ = sqlType (Proxy :: Proxy Double)
