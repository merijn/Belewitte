{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Pretty.Columns
    ( NonEmpty(..)
    , PrettyColumns(prettyColumnInfo)
    , ColumnInfo(..)
    , idColumn
    , column
    , columnVia
    , maybeColumn
    , maybeColumnVia
    , prettyDouble
    , prettyShow
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, maybe)
import Data.Text (Text, pack)
import Database.Persist.Class (EntityField, PersistEntity)

import Sql.Core (Key, SqlBackend, SqlRecord, ToBackendKey, fromSqlKey)

data ColumnInfo rec where
    ColInfo :: EntityField rec v -> (v -> Text) -> ColumnInfo rec

idColumn :: ToBackendKey SqlBackend k => EntityField v (Key k) -> ColumnInfo v
idColumn field = ColInfo field (pack . show . fromSqlKey)

column :: EntityField v Text  -> ColumnInfo v
column field = ColInfo field id

columnVia :: EntityField v r -> (r -> Text) -> ColumnInfo v
columnVia field f = ColInfo field f

maybeColumn :: EntityField v (Maybe Text) -> ColumnInfo v
maybeColumn field = ColInfo field (fromMaybe "")

maybeColumnVia :: EntityField v (Maybe r) -> (r -> Text) -> ColumnInfo v
maybeColumnVia field f = ColInfo field (maybe "" f)

class (PersistEntity a, SqlRecord a) => PrettyColumns a where
    prettyColumnInfo :: NonEmpty (ColumnInfo a)

prettyDouble :: Double -> Text
prettyDouble = pack . show

prettyShow :: Show a => a -> Text
prettyShow = pack . show
