{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Pretty.Fields
    ( NonEmpty(..)
    , FieldAccessor(..)
    , FieldInfo(..)
    , PrettyFields(prettyFieldInfo)
    , NamedEntity(..)
    , optionalPrettyName
    , prettyShow
    , prettyDouble
    , prettyDouble_
    , queryOnlyField
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Text.Format.Numbers (PrettyCfg(..), prettyF)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Class (EntityField)

import Sql.Core (Avg(..), Entity, Max(..), MonadQuery, MonadSql, SqlRecord)

data FieldAccessor ty v where
    PersistField
        :: SqlRecord rec => EntityField rec v -> FieldAccessor (Entity rec) v

    RecordField
        :: (rec -> v)
        -> (forall m . MonadQuery m => m (Avg, Max))
        -> FieldAccessor rec v

data FieldInfo rec where
    VerboseFieldInfo
        :: FieldAccessor rec v
        -> (v -> Text)
        -> (forall m . MonadSql m => Maybe (v -> m Text))
        -> Bool
        -> FieldInfo rec

class PrettyFields a where
    prettyFieldInfo :: NonEmpty (Text, FieldInfo a)

class PrettyFields (Entity a) => NamedEntity a where
    entityName :: a -> Text

optionalPrettyName :: (a -> Maybe Text) -> (a -> Text) -> a -> Text
optionalPrettyName prettyName name v = fromMaybe (name v) $ prettyName v

prettyShow :: Show a => a -> Text
prettyShow = T.pack . show

prettyDouble :: Int -> Double -> Text
prettyDouble n = prettyF PrettyCfg
    { pc_decimals = n
    , pc_thousandsSep = Just ','
    , pc_decimalSep = '.'
    }

prettyDouble_ :: Double -> Text
prettyDouble_ = prettyDouble 2

queryOnlyField :: FieldInfo rec -> FieldInfo rec
queryOnlyField (VerboseFieldInfo f conv convM _) =
  VerboseFieldInfo f conv convM True
