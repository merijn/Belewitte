{-# OPTIONS_GHC -fno-prof-auto #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types
    ( AllowNewer(..)
    , CommitId(..)
    , HashDigest
    , Hash(..)
    , ImplType(..)
    , Percentage
    , getPercentage
    , mkPercentage
    , percent
    , renderPercentage
    , validRational
    ) where

import Crypto.Hash (Digest, MD5, digestFromByteString)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack, unpack)
import Database.Persist.Class (PersistField(..))
import Database.Persist.TH
import Database.Persist.Sql (PersistFieldSql(..))
import Numeric (showFFloat)
import ValidLiterals

data ImplType = Builtin | Core | Derived
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
derivePersistField "ImplType"

data AllowNewer = NoNewer | NewerResults | NewerImpls | AllNewer
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
derivePersistField "AllowNewer"

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
    deriving (Show, Eq, Ord, Lift)

percent :: Real n => n -> n -> Text
percent x y = pack $ showFFloat (Just 2) val "%"
  where
    val :: Double
    val = 100 * realToFrac x / realToFrac y

renderPercentage :: Percentage -> Text
renderPercentage (Percentage d) = percent d 1

mkPercentage :: Double -> Maybe Percentage
mkPercentage = fromLiteral

instance Validate Double Percentage where
    fromLiteralWithError d
        | d < 0 = Left "Percentages can't be smaller than 0"
        | d > 1 = Left "Percentages can't be larger than 1"
        | otherwise = Right . Percentage $ d

instance Validate Rational Percentage where
    fromLiteralWithError d = fromLiteralWithError (fromRational d :: Double)

instance PersistField Percentage where
    toPersistValue = toPersistValue . getPercentage
    fromPersistValue val = fromPersistValue val >>= toPercentage
      where
        toPercentage :: Double -> Either Text Percentage
        toPercentage = first pack . fromLiteralWithError

instance PersistFieldSql Percentage where
    sqlType _ = sqlType (Proxy :: Proxy Double)
