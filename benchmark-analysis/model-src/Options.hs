{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Options (ModelCommand(..), commands, runSqlM) where

import Data.Char (isSpace, toLower)
import qualified Data.Conduit.Combinators as C
import Data.Functor.Compose (Compose(..))
import Data.Foldable (asum)
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Any(..))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Core
import Evaluate
    (CompareReport, EvaluateReport, Report(..), RelativeTo(..), SortBy(..))
import Model (Model)
import OptionParsers
import Query (getDistinctFieldQuery, runSqlQueryConduit)
import Schema
import Sql (SqlField)
import qualified Sql
import Train (TrainingConfig(..))

readProps :: MonadIO m => FilePath -> m (Set Text)
readProps = liftIO . fmap (S.fromList . T.lines) . T.readFile

keepProps :: Set Text -> Set Text -> Set Text
keepProps input db
    | S.null input = db
    | otherwise = S.intersection input db

dropProps :: Set Text -> Set Text -> Set Text
dropProps input db
    | S.null input = db
    | otherwise = S.difference db input

data ModelCommand
    = Train
      { getConfig :: SqlM TrainingConfig }
    | QueryModel
      { getModel :: SqlM (Key PredictionModel, Model) }
    | Validate
      { getAlgoId :: SqlM (Key Algorithm)
      , getPlatformId :: SqlM (Key Platform)
      , getModel :: SqlM (Key PredictionModel, Model)
      }
    | Evaluate
      { getAlgorithm :: SqlM (Entity Algorithm)
      , getPlatformId :: SqlM (Key Platform)
      , getModel :: SqlM (Key PredictionModel, Model)
      , defaultImpl :: Either Int Text
      , evaluateConfig :: EvaluateReport
      }
    | Compare
      { getAlgoId :: SqlM (Key Algorithm)
      , getPlatformId :: SqlM (Key Platform)
      , compareConfig :: CompareReport
      }
    | Export
      { getAlgoId :: SqlM (Key Algorithm)
      , getModel :: SqlM (Key PredictionModel, Model)
      , cppFile :: FilePath
      }
    | QueryTest
      { getAlgoId :: SqlM (Key Algorithm)
      , getPlatformId :: SqlM (Key Platform)
      , outputSuffix :: Maybe FilePath
      }

commands :: String -> Command ModelCommand
commands name = CommandGroup CommandInfo
  { commandName = name
  , commandHeaderDesc = "a tool for generating and validating BDT models"
  , commandDesc =
        "Generate, validate, evaluate, and export Binary Decision Tree (BDT) \
        \models for predicting which implementation to use for an algorithm."
  } [ SingleCommand CommandInfo
        { commandName = "train"
        , commandHeaderDesc = "train a model"
        , commandDesc = "Train a new model"
        } (Train <$> trainingConfig)
    , SingleCommand CommandInfo
        { commandName = "query"
        , commandHeaderDesc = "report model info"
        , commandDesc = "Report model info & statistics"
        } (QueryModel <$> modelParser)
    , SingleCommand CommandInfo
        { commandName = "validate"
        , commandHeaderDesc = "validate model accuracy"
        , commandDesc =
            "Compute and report a model's accuracy on validation dataset and \
            \full dataset"
        } (Validate <$> algorithmIdParser <*> platformIdParser <*> modelParser)
    , SingleCommand CommandInfo
        { commandName = "evaluate"
        , commandHeaderDesc = "evaluate model performance"
        , commandDesc =
            "Evaluate BDT model performance on full dataset and compare \
            \against performance of other implementations"
        }
        $ Evaluate <$> algorithmParser <*> platformIdParser <*> modelParser
                   <*> defaultImplParser <*> evaluateParser
    , SingleCommand CommandInfo
        { commandName = "compare"
        , commandHeaderDesc = "compare implementation performance"
        , commandDesc = "Compare the performance of different implementations"
        }
        $ Compare <$> algorithmIdParser <*> platformIdParser <*> compareParser
    , SingleCommand CommandInfo
        { commandName = "export"
        , commandHeaderDesc = "export model to C++"
        , commandDesc = "Export BDT model to C++ file"
        } (Export <$> algorithmIdParser <*> modelParser <*> cppFile)
    , HiddenCommand CommandInfo
        { commandName = "query-test"
        , commandHeaderDesc = "check query output"
        , commandDesc = "Dump query output to files to validate results"
        }
        $ QueryTest <$> algorithmIdParser <*> platformIdParser
                    <*> (suffixParser <|> pure Nothing)
    ]
  where
    cppFile :: Parser FilePath
    cppFile = strOption . mconcat $
        [ metavar "FILE", short 'e', long "export", value "test.cpp"
        , showDefaultWith id, help "C++ file to write predictor to." ]

    suffixReader :: String -> Maybe (Maybe String)
    suffixReader "" = Nothing
    suffixReader s
        | any isSpace s = Nothing
        | otherwise = Just $ Just s

    suffixParser :: Parser (Maybe String)
    suffixParser = argument (maybeReader suffixReader) . mconcat $
        [ metavar "SUFFIX" ]

defaultImplParser :: Parser (Either Int Text)
defaultImplParser = implParser <|> pure (Right "edge-list")
  where
    implParser = option (Left <$> auto <|> Right <$> str) $ mconcat
        [ metavar "IMPLEMENTATION", short 'i', long "default-impl"
        , help "Default implementation in case of no valid prediction. \
               \Numeric or textual."
        ]

modelParser :: Parser (SqlM (Key PredictionModel, Model))
modelParser = queryModel <$> modelOpt
  where
    modelOpt :: Parser Int64
    modelOpt = option auto $ mconcat
        [ metavar "ID", short 'm', long "model"
        , help "Model to use"
        ]

    queryModel :: Int64 -> SqlM (Key PredictionModel, Model)
    queryModel n = do
        PredictionModel{predictionModelModel} <- Sql.getJust key
        return $ (key, predictionModelModel)
      where
        key = toSqlKey n

reportParser
    :: forall a . Monoid a
    => Map String RelativeTo -> Parser a -> Parser (Report a)
reportParser relTo implTypes =
  Report <$> variantIntervals <*> resultsRelativeTo <*> sortResultsBy
         <*> implTypes <*> detailed
  where
    variantIntervals :: Parser (IntervalSet Int64)
    variantIntervals = IS.unions <$> many intervals
      where
        intervals = intervalFlagRange <|> defaultIntervalFlag

    defaultIntervalFlag :: Parser (IntervalSet Int64)
    defaultIntervalFlag = flag' IS.whole $ mconcat
        [ long "report-all", help "Reports results for all variants." ]

    intervalFlagRange :: Parser (IntervalSet Int64)
    intervalFlagRange = option intervalReader $ mconcat
        [ metavar "RANGE", long "report-range"
        , help "Range(s) of variants to print results for. Accepts \
               \comma-seperated ranges. A range is dash-separated inclusive \
               \range or a single number. Example: \
               \--report-range=5-10,13,17-20" ]

    resultsRelativeTo :: Parser RelativeTo
    resultsRelativeTo = optionParserFromValues relTo "REL-TO" helpTxt $ mconcat
        [ long "rel-to", value Optimal, showDefaultWith (map toLower . show)]
      where
        helpTxt = "Results to normalise result output to."

    sortResultsBy :: Parser SortBy
    sortResultsBy = optionParserFromValues values "SORT-BY" helpTxt $ mconcat
        [ long "sort-by", value Avg, showDefaultWith (map toLower . show) ]
      where
        values = M.fromList [("avg", Avg), ("max", Max)]
        helpTxt = "Time to sort results by."

    detailed :: Parser Bool
    detailed = flag False True $ mconcat
        [ long "detail", help "Show detailed performance stats" ]

defaultRelativeToValues :: Map String RelativeTo
defaultRelativeToValues = M.fromList $
    [("optimal", Optimal), ("best", BestNonSwitching)]

implTypesParser :: Monoid a => (ImplType -> a) -> Map String a -> Parser a
implTypesParser makeResult extraVals = mappend (makeResult Builtin) <$>
    (mconcat <$> some implParser <|> pure (makeResult Core))
  where
    allValues = extraVals <> values
    implParser = optionParserFromValues allValues "TYPE" helpTxt $ mconcat
        [ long "impl-type" ]
      where
        helpTxt = "Implementation types to output results for."

    values = M.fromList $
        [ ("core", makeResult Core)
        , ("derived", makeResult Derived)
        ]

evaluateParser :: Parser EvaluateReport
evaluateParser = reportParser relToValues $ implTypesParser S.singleton M.empty
  where
    relToValues = M.insert "predicted" Predicted defaultRelativeToValues

compareParser :: Parser CompareReport
compareParser = reportParser defaultRelativeToValues implTypes
  where
    implTypes = implTypesParser ((mempty,) . S.singleton) extraVals
    extraVals = M.singleton "comparison" (Any True, S.empty)

type SqlParser = Compose Parser SqlM

trainingConfig :: Parser (SqlM TrainingConfig)
trainingConfig = getCompose $
    TrainConfig <$> props GraphPropProperty "graph"
                <*> props StepPropProperty "step"
                <*> trainFract
                <*> seedOpt
  where
    seedOpt = Compose . fmap pure . option auto . mconcat $
        [ metavar "N", short 's', long "seed", value 42, showDefault
        , help "Seed for training set randomisation" ]

    trainFract = Compose . fmap pure . option auto . mconcat $
        [ metavar "PERCENT", short 'p', long "percent", value 0.8, showDefault
        , help "Training set as percentage of data." ]

    props
        :: SqlField rec Text
        => Sql.EntityField rec Text -> String -> SqlParser (Set Text)
    props field name = asum
        [keepFilter field name, dropFilter field name, gatherProps field]

    keepFilter
        :: SqlField rec Text
        => Sql.EntityField rec Text -> String -> SqlParser (Set Text)
    keepFilter field name =
        keepProps <$> gatherProps field <*> Compose (readProps <$> keepOpt)
      where
        keepOpt = strOption $ mconcat
            [ metavar "FILE", long ("keep-" <> name <> "-props")
            , help "File listing properties to use for training, one per line."
            ]

    dropFilter
        :: SqlField rec Text
        => Sql.EntityField rec Text -> String -> SqlParser (Set Text)
    dropFilter field name =
        dropProps <$> gatherProps field <*> Compose (readProps <$> dropOpt)
      where
        dropOpt = strOption $ mconcat
            [ metavar "FILE", long ("drop-" <> name <> "-props")
            , help "File listing properties not to use for training, \
                   \one per line."]

    gatherProps
        :: (SqlField rec Text)
        => Sql.EntityField rec Text -> SqlParser (Set Text)
    gatherProps field = Compose . pure $ do
        query <- getDistinctFieldQuery field
        runSqlQueryConduit query $ C.foldMap S.singleton
