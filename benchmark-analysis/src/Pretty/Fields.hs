{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Pretty.Fields
    ( NonEmpty(..)
    , PrettyFields(prettyFieldInfo)
    , FieldInfo(..)
    , idField
    , textField
    , fieldVia
    , maybeTextField
    , maybeFieldVia
    , multilineTextField
    , prettyDouble
    , prettyShow
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, maybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Class (EntityField, PersistEntity)

import Sql.Core (Key, SqlBackend, SqlRecord, ToBackendKey, fromSqlKey)

data FieldInfo rec where
    FieldInfo :: EntityField rec v -> (v -> Text) -> FieldInfo rec

idField :: ToBackendKey SqlBackend k => EntityField v (Key k) -> FieldInfo v
idField f = FieldInfo f (T.pack . show . fromSqlKey)

textField :: EntityField v Text -> FieldInfo v
textField f = FieldInfo f id

fieldVia :: EntityField v r -> (r -> Text) -> FieldInfo v
fieldVia f conv = FieldInfo f conv

maybeTextField :: EntityField v (Maybe Text) -> FieldInfo v
maybeTextField f = FieldInfo f (fromMaybe "")

multilineTextField :: EntityField v (Maybe Text) -> FieldInfo v
multilineTextField f = FieldInfo f (maybe "" format)
  where
    format :: Text -> Text
    format = mappend "\n" . T.unlines . map ("    " <>) . T.lines

maybeFieldVia :: EntityField v (Maybe r) -> (r -> Text) -> FieldInfo v
maybeFieldVia f conv = FieldInfo f (maybe "" conv)

class (PersistEntity a, SqlRecord a) => PrettyFields a where
    prettyFieldInfo :: NonEmpty (Text, FieldInfo a)

prettyDouble :: Double -> Text
prettyDouble = T.pack . show

prettyShow :: Show a => a -> Text
prettyShow = T.pack . show
