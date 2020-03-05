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
    , queryOnlyField
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Class (EntityField, PersistEntity)

import Sql.Core (Key, SqlBackend, SqlRecord, ToBackendKey, fromSqlKey)

data FieldInfo rec where
    FieldInfo :: EntityField rec v -> (v -> Text) -> Bool -> FieldInfo rec

idField :: ToBackendKey SqlBackend k => EntityField v (Key k) -> FieldInfo v
idField f = FieldInfo f (T.pack . show . fromSqlKey) False

textField :: EntityField v Text -> FieldInfo v
textField f = FieldInfo f id False

fieldVia :: EntityField v r -> (r -> Text) -> FieldInfo v
fieldVia f conv = FieldInfo f conv False

maybeTextField :: EntityField v (Maybe Text) -> FieldInfo v
maybeTextField f = FieldInfo f (fromMaybe "") False

multilineTextField :: EntityField v (Maybe Text) -> FieldInfo v
multilineTextField f = FieldInfo f (maybe "" format) True
  where
    format :: Text -> Text
    format = mappend "\n" . T.unlines . map ("    " <>) . T.lines

maybeFieldVia :: EntityField v (Maybe r) -> (r -> Text) -> FieldInfo v
maybeFieldVia f conv = FieldInfo f (maybe "" conv) False

class (PersistEntity a, SqlRecord a) => PrettyFields a where
    prettyFieldInfo :: NonEmpty (Text, FieldInfo a)

prettyDouble :: Double -> Text
prettyDouble = T.pack . show

prettyShow :: Show a => a -> Text
prettyShow = T.pack . show

queryOnlyField :: FieldInfo rec -> FieldInfo rec
queryOnlyField (FieldInfo f conv _) = FieldInfo f conv True
