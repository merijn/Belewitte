{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pretty.Fields.Persistent
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
import Database.Persist.Class (EntityField)

import Pretty.Fields
import Sql.Core (Entity, Key, MonadSql, SqlBackend, SqlRecord, ToBackendKey)
import qualified Sql.Core as Sql

pattern FieldInfo
    :: SqlRecord rec
    => EntityField rec v -> (v -> Text) -> Bool -> FieldInfo (Entity rec)
pattern FieldInfo field conv b <- VerboseFieldInfo (PersistField field) conv _ b where
    FieldInfo field conv b = VerboseFieldInfo (PersistField field) conv Nothing b

idField
    :: (ToBackendKey SqlBackend k, SqlRecord v)
    => EntityField v (Key k) -> FieldInfo (Entity v)
idField f = FieldInfo f (T.pack . show . Sql.fromSqlKey) False

namedIdField
    :: forall k v
     . (ToBackendKey SqlBackend k, NamedEntity k, SqlRecord v)
     => EntityField v (Key k) -> FieldInfo (Entity v)
namedIdField f = VerboseFieldInfo (PersistField f) showKey (Just prettyName) False
  where
    showKey :: Key k -> Text
    showKey = T.pack . show . Sql.fromSqlKey

    prettyName :: MonadSql m => Key k -> m Text
    prettyName k = do
        value <- Sql.getJust k
        return $ entityName value <> " (#" <> showKey k <> ")"

textField :: SqlRecord v => EntityField v Text -> FieldInfo (Entity v)
textField f = FieldInfo f id False

doubleField
    :: SqlRecord v => Int -> EntityField v Double -> FieldInfo (Entity v)
doubleField n f = VerboseFieldInfo (PersistField f) prettyShow (Just mkPretty) False
  where
    mkPretty :: Monad m => Double -> m Text
    mkPretty = return . prettyDouble n

doubleField_ :: SqlRecord v => EntityField v Double -> FieldInfo (Entity v)
doubleField_ = doubleField 2

fieldVia
    :: SqlRecord v => EntityField v r -> (r -> Text) -> FieldInfo (Entity v)
fieldVia f conv = FieldInfo f conv False

maybeTextField
    :: SqlRecord v => EntityField v (Maybe Text) -> FieldInfo (Entity v)
maybeTextField f = FieldInfo f (fromMaybe "") False

multilineTextField
    :: SqlRecord v => EntityField v (Maybe Text) -> FieldInfo (Entity v)
multilineTextField f = FieldInfo f (maybe "" format) True
  where
    format :: Text -> Text
    format = mappend "\n" . T.unlines . map ("    " <>) . T.lines

maybeFieldVia
    :: SqlRecord v
    => EntityField v (Maybe r) -> (r -> Text) -> FieldInfo (Entity v)
maybeFieldVia f conv = FieldInfo f (maybe "" conv) False
