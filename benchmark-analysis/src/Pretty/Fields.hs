{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pretty.Fields
    ( NonEmpty(..)
    , PrettyFields(prettyFieldInfo)
    , NamedEntity(entityName)
    , FieldInfo(..)
    , pattern FieldInfo
    , idField
    , namedIdField
    , textField
    , doubleField
    , doubleField_
    , fieldVia
    , maybeTextField
    , maybeFieldVia
    , multilineTextField
    , optionalPrettyName
    , prettyDouble
    , prettyDouble_
    , prettyShow
    , queryOnlyField
    ) where

import Data.Text.Format.Numbers (PrettyCfg(..), prettyF)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Class (EntityField)

import Sql.Core
    (Key, MonadSql, SqlBackend, SqlRecord, ToBackendKey, fromSqlKey, getJust)

pattern FieldInfo :: EntityField rec v -> (v -> Text) -> Bool -> FieldInfo rec
pattern FieldInfo field conv b <- VerboseFieldInfo field conv _ b where
    FieldInfo field conv b = VerboseFieldInfo field conv Nothing b

data FieldInfo rec where
    VerboseFieldInfo
        :: EntityField rec v
        -> (v -> Text)
        -> (forall m . MonadSql m => Maybe (v -> m Text))
        -> Bool
        -> FieldInfo rec

idField :: ToBackendKey SqlBackend k => EntityField v (Key k) -> FieldInfo v
idField f = FieldInfo f (T.pack . show . fromSqlKey) False

namedIdField
    :: forall k v
     . (ToBackendKey SqlBackend k, NamedEntity k)
     => EntityField v (Key k) -> FieldInfo v
namedIdField f = VerboseFieldInfo f showKey (Just prettyName) False
  where
    showKey :: Key k -> Text
    showKey = T.pack . show . fromSqlKey

    prettyName :: MonadSql m => Key k -> m Text
    prettyName k = do
        value <- getJust k
        return $ entityName value <> " (#" <> showKey k <> ")"

textField :: EntityField v Text -> FieldInfo v
textField f = FieldInfo f id False

doubleField :: Int -> EntityField v Double -> FieldInfo v
doubleField n f = VerboseFieldInfo f prettyShow (Just mkPretty) False
  where
    mkPretty :: Monad m => Double -> m Text
    mkPretty = return . prettyDouble n

doubleField_ :: EntityField v Double -> FieldInfo v
doubleField_ = doubleField 2

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

class SqlRecord a => PrettyFields a where
    prettyFieldInfo :: NonEmpty (Text, FieldInfo a)

optionalPrettyName :: (a -> Maybe Text) -> (a -> Text) -> a -> Text
optionalPrettyName prettyName name v = fromMaybe (name v) $ prettyName v

class PrettyFields a => NamedEntity a where
    entityName :: a -> Text

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
