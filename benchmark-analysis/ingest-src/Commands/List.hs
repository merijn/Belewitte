{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Commands.List (commands) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M
import Options.Applicative.Help ((</>))

import Commands
import Core
import FormattedOutput (renderColumns)
import OptionParsers
import Pretty.Fields
import Schema
import Sql (Filter, PersistEntity, PersistField, SelectOpt(..), (==.), (||.))
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

fromField :: (forall v . EntityField rec v -> a) -> Field rec x -> a
fromField f (Simple field) = f field
fromField f (Optional field) = f field

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

    fieldLike :: Field rec Text -> Text -> Filter rec
    fieldLike (Simple f) val = f `Sql.likeFilter` val
    fieldLike (Optional f) val = f `Sql.likeFilter` val

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
     . (PersistEntity rec, PrettyFields rec)
    => Either (FieldSpec rec) [AnyField rec]
    -> [(String, FieldSpec rec)]
    -> Parser (SqlM ())
buildOptionsWithOptionalId idKey (M.fromList -> optMap) =
    renderColumns <$> filters <*> selections
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
    :: (PersistEntity r, PrettyFields r, Sql.ToBackendKey Sql.SqlBackend r)
    => [(String, FieldSpec r)] -> Parser (SqlM ())
buildOptions = buildOptionsWithOptionalId (Left recordIdField)
  where
    recordIdField = IdField '\0' $ Simple Sql.persistIdField

buildOptionsWithoutId
    :: (PersistEntity rec, PrettyFields rec)
    => [AnyField rec] -> [(String, FieldSpec rec)] -> Parser (SqlM ())
buildOptionsWithoutId = buildOptionsWithOptionalId . Right

commands :: Command (SqlM ())
commands = CommandGroup CommandInfo
  { commandName = "list"
  , commandHeaderDesc = "list database entries"
  , commandDesc =
      "List database entries of various information stored in the database."
  } [ SingleCommand CommandInfo
        { commandName = "algorithm"
        , commandHeaderDesc = "list registered algorithms"
        , commandDesc = "List all registered algorithms."
        }
        $ buildOptions
            [ "name" =. StringField 'n' $ Simple AlgorithmName
            , "pretty-name" =. StringField 'p' $ Optional AlgorithmPrettyName
            ]
    , SingleCommand CommandInfo
        { commandName = "dataset"
        , commandHeaderDesc = "list registered datasets"
        , commandDesc = "List all registered datasets."
        }
        $ buildOptions ([] :: [(String, FieldSpec Dataset)])
    , SingleCommand CommandInfo
        { commandName = "external-impl"
        , commandHeaderDesc = "list external implementations"
        , commandDesc = "List all external implementations."
        }
        $ buildOptions
            [ "algorith" =. IdField 'a' $ Simple ExternalImplAlgorithmId
            , "name" =. StringField 'n' $ Simple ExternalImplName
            , "pretty-name" =. StringField 'p' $ Optional ExternalImplPrettyName
            ]
    , SingleCommand CommandInfo
        { commandName = "external-timer"
        , commandHeaderDesc = "list timings of external implementations"
        , commandDesc = "List all timings of external implementations."
        }
        $ buildOptionsWithoutId
            [ AnyField ExternalTimerPlatformId
            , AnyField ExternalTimerVariantId
            , AnyField ExternalTimerImplId
            ]

            [ "platform" =. IdField 'p' $ Simple ExternalTimerPlatformId
            , "variant" =. IdField 'v' $ Simple ExternalTimerVariantId
            , "implementation" =. IdField 'i' $ Simple ExternalTimerImplId
            , "algorithm" =. IdField 'a' $ Simple ExternalTimerAlgorithmId
            , "name" =. StringField 'n' $ Simple ExternalTimerName
            , "min" =. SortOnlyField $ ExternalTimerMinTime
            , "avg" =. SortOnlyField $ ExternalTimerAvgTime
            , "max" =. SortOnlyField $ ExternalTimerMaxTime
            , "std-dev" =. SortOnlyField $ ExternalTimerStdDev
            ]
    , SingleCommand CommandInfo
        { commandName = "graph"
        , commandHeaderDesc = "list graphs"
        , commandDesc = "List all graphs."
        }
        $ buildOptions
            [ "name" =. StringField 'n' $ Simple GraphName
            , "dataset" =. IdField 'd' $ Simple GraphDatasetId
            , "pretty-name" =. StringField 'r' $ Optional GraphPrettyName
            , "path" =. StringField 'p' $ Simple GraphPath
            ]
    , SingleCommand CommandInfo
        { commandName = "implementation"
        , commandHeaderDesc = "list implementations"
        , commandDesc = "List all implementations."
        }
        $ buildOptions
            [ "algorithm" =. IdField 'a' $ Simple ImplementationAlgorithmId
            , "name" =. StringField 'n' $ Simple ImplementationName
            , "type" =. EnumField 't' $ Simple ImplementationType
            , "pretty-name" =. StringField 'p' $ Optional ImplementationPrettyName
            ]
    , SingleCommand CommandInfo
        { commandName = "platform"
        , commandHeaderDesc = "list platforms"
        , commandDesc = "List all platforms."
        }
        $ buildOptions
            [ "name" =. StringField 'n' $ Simple PlatformName
            , "pretty-name" =. StringField 'p' $ Optional PlatformPrettyName
            , "available" =. SortOnlyField $ PlatformAvailable
            , "default" =. SortOnlyField $ PlatformIsDefault
            ]
    , SingleCommand CommandInfo
        { commandName = "graph-properties"
        , commandHeaderDesc = "list graph properties"
        , commandDesc = "List all graph properties."
        }
        $ buildOptionsWithoutId
            [AnyField GraphPropGraphId, AnyField GraphPropProperty]

            [ "graph" =. IdField 'g' $ Simple GraphPropGraphId
            , "property" =. StringField 'p' $ Simple GraphPropProperty
            , "value" =. SortOnlyField $ GraphPropValue
            ]
    , SingleCommand CommandInfo
        { commandName = "step-properties"
        , commandHeaderDesc = "list step properties"
        , commandDesc = "List all step properties."
        }
        $ buildOptionsWithoutId
            [ AnyField StepPropVariantId
            , AnyField StepPropStepId
            , AnyField StepPropProperty
            ]

            [ "variant" =. IdField 'v' $ Simple StepPropVariantId
            , "step" =. IntField 's' $ Simple StepPropStepId
            , "property" =. StringField 'p' $ Simple StepPropProperty
            , "value" =. SortOnlyField $ StepPropValue
            ]
    , SingleCommand CommandInfo
        { commandName = "run-config"
        , commandHeaderDesc = "list run configs"
        , commandDesc = "List all run configs."
        }
        $ buildOptions
            [ "algorithm" =. IdField 'a' $ Simple RunConfigAlgorithmId
            , "platform" =. IdField 'p' $ Simple RunConfigPlatformId
            , "dataset" =. IdField 'd' $ Simple RunConfigDatasetId
            , "commit" =. StringField 'c' $ Simple RunConfigAlgorithmVersion
            , "repeats" =. SortOnlyField $ RunConfigRepeats
            ]
    , SingleCommand CommandInfo
        { commandName = "run"
        , commandHeaderDesc = "list runs"
        , commandDesc = "List all runs."
        }
        $ buildOptions
            [ "run-config" =. IdField 'r' $ Simple RunRunConfigId
            , "algorithm" =. IdField 'a' $ Simple RunAlgorithmId
            , "variant" =. IdField 'v' $ Simple RunVariantId
            , "implementation" =. IdField 'i' $ Simple RunImplId
            , "validated" =. EnumField 'l' $ Simple RunValidated
            , "time" =. SortOnlyField $ RunTimestamp
            ]
    , SingleCommand CommandInfo
        { commandName = "timer"
        , commandHeaderDesc = "list global timers"
        , commandDesc = "List all global timers."
        }
        $ buildOptionsWithoutId
            [AnyField TotalTimerRunId, AnyField TotalTimerName]

            [ "run" =. IdField 'r' $ Simple TotalTimerRunId
            , "name" =. StringField 'n' $ Simple TotalTimerName
            , "min" =. SortOnlyField $ TotalTimerMinTime
            , "avg" =. SortOnlyField $ TotalTimerAvgTime
            , "max" =. SortOnlyField $ TotalTimerMaxTime
            , "stddev" =. SortOnlyField $ TotalTimerStdDev
            ]
    , SingleCommand CommandInfo
        { commandName = "step-timer"
        , commandHeaderDesc = "list step timers"
        , commandDesc = "List all step timers."
        }
        $ buildOptionsWithoutId
            [ AnyField StepTimerRunId
            , AnyField StepTimerStepId
            , AnyField StepTimerName
            ]

            [ "run" =. IdField 'r' $ Simple StepTimerRunId
            , "step" =. IntField 's' $ Simple StepTimerStepId
            , "name" =. StringField 'n' $ Simple StepTimerName
            , "min" =. SortOnlyField $ StepTimerMinTime
            , "avg" =. SortOnlyField $ StepTimerAvgTime
            , "max" =. SortOnlyField $ StepTimerMaxTime
            , "stddev" =. SortOnlyField $ StepTimerStdDev
            ]
    , SingleCommand CommandInfo
        { commandName = "variant-config"
        , commandHeaderDesc = "list variant configs"
        , commandDesc = "List all variant configs."
        }
        $ buildOptions
            [ "name" =. StringField 'n' $ Simple VariantConfigName
            , "algorithm" =. IdField 'a' $ Simple VariantConfigAlgorithmId
            , "default" =. SortOnlyField $ VariantConfigIsDefault
            ]
    , SingleCommand CommandInfo
        { commandName = "variant"
        , commandHeaderDesc = "list registered variant"
        , commandDesc = "List all registered variants."
        }
        $ buildOptions
            [ "variantconfig" =. IdField 'v' $ Simple VariantVariantConfigId
            , "algorithm" =. IdField 'a' $ Simple VariantAlgorithmId
            , "graph" =. IdField 'g' $ Simple VariantGraphId
            , "props-stored" =. EnumField 'p' $ Simple VariantPropsStored
            ]
    ]
