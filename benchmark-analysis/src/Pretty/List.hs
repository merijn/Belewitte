{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Pretty.List
    ( AnyField(..)
    , Field(..)
    , FieldSpec(..)
    , (=.)
    , buildOptions
    , buildOptionsWithoutId
    ) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M
import Options.Applicative.Help ((</>))

import Core
import FormattedOutput (renderColumnsSelect)
import Options
import Pretty.Fields
import Sql
    ( EntityField
    , Filter
    , PersistEntity
    , PersistField
    , SelectOpt(..)
    , SqlRecord
    , (==.)
    , (||.)
    )
import qualified Sql

(<:>) :: (Alternative f, Applicative f) => f a -> f [a] -> f [a]
x <:> xs = xOrEmpty <*> xs
  where
    xOrEmpty = ((:) <$> x) <|> pure id
infixr <:>

(=.) :: String -> (v -> FieldSpec rec) -> v -> (String, FieldSpec rec)
(=.) name f field = (name, f field)

data AnyField rec where
    AnyField :: EntityField rec v -> AnyField rec

fromAnyField :: (forall v . EntityField rec v -> a) -> AnyField rec -> a
fromAnyField f (AnyField field) = f field

data Field rec v where
    Simple :: EntityField rec v -> Field rec v
    Optional :: EntityField rec (Maybe v) -> Field rec v
    Converted :: PersistField x => EntityField rec x -> (v -> x) -> Field rec v

fromField :: (forall v . EntityField rec v -> a) -> Field rec x -> a
fromField f (Simple field) = f field
fromField f (Optional field) = f field
fromField f (Converted field _) = f field

data FieldSpec rec where
    IdField
        :: (Sql.ToBackendKey Sql.SqlBackend v, PersistEntity v)
        => Char -> Field rec (Key v) -> FieldSpec rec

    EnumField
        :: (Bounded v, Enum v, PersistField v, Show v)
        => Char -> Field rec v -> FieldSpec rec

    IntField ::  Char -> Field rec Int -> FieldSpec rec

    StringField :: Char -> Field rec Text -> FieldSpec rec

    SortOnlyField
        :: EntityField rec v -> FieldSpec rec

fromFieldSpec :: (forall v . EntityField rec v -> a) -> FieldSpec rec -> a
fromFieldSpec f fieldSpec = case fieldSpec of
        IdField _ field -> fromField f field
        EnumField _ field -> fromField f field
        IntField _ field -> fromField f field
        StringField _ field -> fromField f field
        SortOnlyField field -> f field

offsetOption :: Parser (SelectOpt v)
offsetOption = option (OffsetBy <$> auto) $ mconcat
    [ metavar "N", long "offset", help "Skip the first N entries." ]

limitOption :: Parser (SelectOpt v)
limitOption = option (LimitTo <$> auto) $ mconcat
    [ metavar "N", long "limit", help "Show at most N entries." ]

toFilter :: forall rec . String -> FieldSpec rec -> Parser [Filter rec]
toFilter name field = parseMultiple <|> pure []
  where
    parseMultiple :: Parser [Filter rec]
    parseMultiple = foldr1 (||.) . map pure <$> some (optParser field)

    showLower :: Show a => a -> String
    showLower = map toLower . show

    fieldEq :: PersistField v => Field rec v -> v -> Filter rec
    fieldEq (Simple f) val = f ==. val
    fieldEq (Optional f) val = f ==. Just val
    fieldEq (Converted f convert) val = f ==. convert val

    fieldLike :: Field rec Text -> Text -> Filter rec
    fieldLike (Simple f) val = f `Sql.likeFilter` val
    fieldLike (Optional f) val = f `Sql.likeFilter` val
    fieldLike (Converted f _) val = f `Sql.likeFilter` val

    optParser :: FieldSpec rec -> Parser (Filter rec)
    optParser (IdField c f) = fieldEq f . toSqlKey <$> option auto config
      where
        config = mconcat [metavar "ID", short c, long name, idHelp]
        idHelp = help "Restrict output to lines referencing ID"

    optParser (EnumField c f) = fieldEq f <$> option (readCI vals) config
      where
        vals = [minBound .. maxBound]
        config = mconcat [metavar "VAL", short c, long name, enumHelp]
        enumHelp = helpDoc . Just $
            "Restrict output to lines having VAL."
            </> optionEnumerationHelp "VAL" (map showLower vals)

    optParser (IntField c f) = fieldEq f <$> option auto config
      where
        config = mconcat [metavar "N", short c, long name, help helpText]
        helpText = "Restrict output to lines equaling N."

    optParser (StringField c f) = fieldLike f <$> strOption config
      where
        config = mconcat [metavar "STRING", short c, long name, help helpText]
        helpText = "Restrict output to lines that match STRING."

    optParser (SortOnlyField _) = empty

buildOptionsWithOptionalId
    :: forall rec
     . (PrettyFields (Entity rec), SqlRecord rec)
    => Either (FieldSpec rec) [AnyField rec]
    -> [(String, FieldSpec rec)]
    -> Parser (SqlM ())
buildOptionsWithOptionalId idKey (M.fromList -> optMap) =
    renderColumnsSelect <$> filters <*> selections
  where
    filters :: Parser [Filter rec]
    filters = mconcat . M.elems <$> M.traverseWithKey toFilter optMap

    selections :: Parser [SelectOpt rec]
    selections = offsetOption <:> limitOption <:> sorting

    extOptMap :: Map String (FieldSpec rec)
    extOptMap = case idKey of
        Left field -> M.insert "id" field optMap
        Right _ -> optMap

    optionNames :: [String]
    optionNames = map (map toLower) $ M.keys extOptMap

    sorting :: Parser [SelectOpt rec]
    sorting = some (sortAsc <|> sortDesc) <|> pure defaultSort
      where
        defaultSort = case idKey of
            Left field -> [fromFieldSpec Asc field]
            Right fields -> map (fromAnyField Asc) fields

    sortAsc :: Parser (SelectOpt rec)
    sortAsc = fmap (fromFieldSpec Asc) . mapOption extOptMap $ mconcat
        [ metavar "NAME", long "sort-asc", helpDoc (Just helpText) ]
      where
        helpText = "Sort output in ascending order of specified column(s)."
               </> optionEnumerationHelp "NAME" optionNames

    sortDesc :: Parser (SelectOpt rec)
    sortDesc = fmap (fromFieldSpec Desc) . mapOption extOptMap $ mconcat
        [ metavar "NAME", long "sort-desc", helpDoc (Just helpText) ]
      where
        helpText = "Sort output in descending order of specified column(s)."
               </> optionEnumerationHelp "NAME" optionNames

buildOptions
    :: (PrettyFields (Entity r), Sql.ToBackendKey Sql.SqlBackend r)
    => [(String, FieldSpec r)] -> Parser (SqlM ())
buildOptions = buildOptionsWithOptionalId (Left recordIdField)
  where
    recordIdField = IdField '\0' $ Simple Sql.persistIdField

buildOptionsWithoutId
    :: (PrettyFields (Entity rec), SqlRecord rec)
    => [AnyField rec] -> [(String, FieldSpec rec)] -> Parser (SqlM ())
buildOptionsWithoutId = buildOptionsWithOptionalId . Right
