{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pretty.Fields.Record
    ( module Pretty.Fields
    , idField
    , namedIdField
    , textField
    , doubleField
    , doubleField_
    , fieldVia
    , maybeTextField
    , multilineTextField
    , maybeFieldVia
    ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Pretty.Fields
import Query (MonadQuery)
import Sql
    (Avg(..), EntityField, Key, Max(..), MonadSql, SqlBackend, ToBackendKey)
import qualified Sql as Sql

pattern FieldInfo
    :: (forall m . MonadQuery m => m (Avg, Max))
    -> (rec -> v)
    -> (v -> Text)
    -> Bool
    -> FieldInfo rec
pattern FieldInfo width field conv b <-
        VerboseFieldInfo (RecordField field width) conv _ b where

    FieldInfo width field conv b =
        VerboseFieldInfo (RecordField field width) conv Nothing b

idField
    :: forall k v . (ToBackendKey SqlBackend k) => (v -> Key k) -> FieldInfo v
idField f = FieldInfo width f (T.pack . show . Sql.fromSqlKey) False
  where
    width :: MonadQuery m => m (Avg, Max)
    width = Sql.getFieldLength (Sql.persistIdField :: EntityField k (Key k))

namedIdField
    :: forall k v
     . (ToBackendKey SqlBackend k, NamedEntity k)
     => (v -> Key k) -> FieldInfo v
namedIdField f =
    VerboseFieldInfo (RecordField f width) showKey (Just prettyName) False
  where
    width :: MonadQuery m => m (Avg, Max)
    width = Sql.getFieldLength (Sql.persistIdField :: EntityField k (Key k))

    showKey :: Key k -> Text
    showKey = T.pack . show . Sql.fromSqlKey

    prettyName :: MonadSql m => Key k -> m Text
    prettyName k = do
        value <- Sql.getJust k
        return $ entityName value <> " (#" <> showKey k <> ")"

constWidth :: Monad m => Int -> m (Avg, Max)
constWidth n = return (Avg n, Max n)

textField :: Int -> (v -> Text) -> FieldInfo v
textField n f = FieldInfo (constWidth n) f id False

doubleField
    :: Int -> (v -> Double) -> FieldInfo v
doubleField n f = VerboseFieldInfo (RecordField f width) prettyShow (Just mkPretty) False
  where
    mkPretty :: Monad m => Double -> m Text
    mkPretty = return . prettyDouble n

    width :: Monad m => m (Avg, Max)
    width = return (Avg (3+n), Max (5+n))

doubleField_ :: (v -> Double) -> FieldInfo v
doubleField_ = doubleField 2

fieldVia
    :: (v -> r) -> (r -> Text) -> Int -> FieldInfo v
fieldVia f conv n = FieldInfo (constWidth n) f conv False

maybeTextField
    :: Int -> (v -> Maybe Text) -> FieldInfo v
maybeTextField n f = FieldInfo (constWidth n) f (fromMaybe "") False

multilineTextField
    :: (v -> Maybe Text) -> FieldInfo v
multilineTextField f = FieldInfo (constWidth 0) f (maybe "" format) True
  where
    format :: Text -> Text
    format = mappend "\n" . T.unlines . map ("    " <>) . T.lines

maybeFieldVia
    :: (v -> Maybe r) -> (r -> Text) -> Int -> FieldInfo v
maybeFieldVia f conv n = FieldInfo (constWidth n) f (maybe "" conv) False
