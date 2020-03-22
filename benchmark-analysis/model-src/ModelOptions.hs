{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module ModelOptions (ModelCommand(..), commands, runSqlM) where

import Data.Char (toLower)
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
import FieldQuery (getDistinctFieldQuery)
import Options
import Pretty.List (Field(..), FieldSpec(..), (=.), buildOptions)
import Query (runSqlQueryConduit)
import QueryDump (modelQueryDump)
import Schema
import Sql ((==.))
import qualified Sql
import StepQuery (QueryMode, StepInfoConfig(..), stepInfoQuery)
import qualified StepQuery
import VariantQuery

data ModelCommand
    = Train
      { getConfig :: SqlM StepInfoConfig }
    | QueryModel
      { getModelEntity :: SqlM (Entity PredictionModel) }
    | ListModels
      { listModels :: SqlM () }
    | ValidateModel
      { getModelId :: SqlM (Key PredictionModel)
      , getOptionalPlatformId :: SqlM (Maybe (Key Platform))
      , getOptionalDatasetIds :: SqlM (Maybe (Set (Key Dataset)))
      }
    | EvaluatePredictor
      { getPlatformId :: SqlM (Key Platform)
      , getModelId :: SqlM (Key PredictionModel)
      , defaultImpl :: Either Int Text
      , evaluateConfig :: EvaluateReport
      , getDatasetIds :: SqlM (Set (Key Dataset))
      }
    | Compare
      { getVariantInfoConfig :: SqlM VariantInfoConfig
      , compareConfig :: CompareReport
      }
    | ExportModel
      { getModel :: SqlM (Entity PredictionModel)
      , cppFile :: FilePath
      }

commands :: CommandRoot ModelCommand
commands = CommandRoot
  { mainHeaderDesc = "a tool for generating and validating BDT models"
  , mainDesc =
        "Generate, validate, evaluate, and export Binary Decision Tree (BDT) \
        \models for predicting which implementation to use for an algorithm."
  , mainQueryDump = modelQueryDump
  , mainQueryMap = modelQueryMap
  , mainCommands =
    [ SingleCommand CommandInfo
        { commandName = "train"
        , commandHeaderDesc = "train a model"
        , commandDesc = "Train a new model"
        } (Train <$> stepInfoConfig)
    , SingleCommand CommandInfo
        { commandName = "query"
        , commandHeaderDesc = "report model info"
        , commandDesc = "Report model info & statistics"
        } (QueryModel <$> modelParser)
    , SingleCommand CommandInfo
        { commandName = "list"
        , commandHeaderDesc = "list trained models"
        , commandDesc = "List all trained models."
        }
        $ ListModels <$> buildOptions
            [ "platform" =. IdField 'p' $ Simple PredictionModelPlatformId
            , "algorithm" =. IdField 'a' $ Simple PredictionModelAlgorithmId
            , "commit" =. StringField 'c' $
                Converted PredictionModelAlgorithmVersion CommitId
            , "name" =. StringField 'n' $ Simple PredictionModelName
            , "pretty-name" =. StringField 'r' $
                Optional PredictionModelPrettyName
            , "train-seed" =. SortOnlyField $ PredictionModelTrainSeed
            , "train-legacy" =. SortOnlyField $
                PredictionModelLegacyTrainFraction
            , "train-graphs" =. SortOnlyField $ PredictionModelTrainGraphs
            , "train-variants" =. SortOnlyField $ PredictionModelTrainVariants
            , "train-steps" =. SortOnlyField $ PredictionModelTrainSteps
            , "unknown-count" =. SortOnlyField $
                PredictionModelTotalUnknownCount
            , "time" =. SortOnlyField $ PredictionModelTimestamp
            ]
    , SingleCommand CommandInfo
        { commandName = "validate"
        , commandHeaderDesc = "validate model accuracy"
        , commandDesc =
            "Compute and report a model's accuracy on validation dataset and \
            \full dataset"
        }
        $ ValidateModel
            <$> modelIdParser <*> (sequence <$> optional platformIdParser)
            <*> (sequence <$> optional datasetsParser)
    , SingleCommand CommandInfo
        { commandName = "evaluate"
        , commandHeaderDesc = "evaluate model performance"
        , commandDesc =
            "Evaluate BDT model performance on full dataset and compare \
            \against performance of other implementations"
        }
        $ EvaluatePredictor
            <$> platformIdParser <*> modelIdParser <*> defaultImplParser
            <*> evaluateParser <*> datasetsParser
    , SingleCommand CommandInfo
        { commandName = "compare"
        , commandHeaderDesc = "compare implementation performance"
        , commandDesc = "Compare the performance of different implementations"
        }
        $ Compare <$> variantInfoConfigParser <*> compareParser
    , SingleCommand CommandInfo
        { commandName = "export"
        , commandHeaderDesc = "export model to C++"
        , commandDesc = "Export BDT model to C++ file"
        } (ExportModel <$> modelParser <*> cppFile)
    ]
  }
  where
    cppFile :: Parser FilePath
    cppFile = strOption . mconcat $
        [ metavar "FILE", short 'e', long "export", value "test.cpp"
        , showDefaultWith id, help "C++ file to write predictor to." ]

datasetsParser :: Parser (SqlM (Set (Key Dataset)))
datasetsParser = fmap S.fromList . sequence <$> many datasetIdParser

graphPercentageParser :: Parser Percentage
graphPercentageParser = percentageParser
    [ long "graph-percent"
    , help "Percentage of graphs to include in training set."
    ]

variantPercentageParser :: Parser Percentage
variantPercentageParser = percentageParser
    [ long "variant-percent"
    , help "Percentage of variants to include in training set."
    ]

stepPercentageParser :: Parser Percentage
stepPercentageParser = percentageParser
    [ long "step-percent"
    , help "Percentage of steps to include in training set."
    , value ($$(validRational 0.8) :: Percentage)
    ]

modelQueryMap :: Map String (Parser DebugQuery)
modelQueryMap = M.fromList
    [ nameDebugQuery "stepInfoQuery" $ Compose $ do
        getAlgoId <- algorithmIdParser
        getPlatformId <- platformIdParser
        getCommitId <- commitIdParser
        getUtcTime <- utcTimeParser
        queryMode <- queryModeParser
        trainSeed <- trainSeedParser
        getDatasets <- datasetsParser
        shouldFilter <- filterIncomplete
        graphPercent <- graphPercentageParser
        variantPercent <- variantPercentageParser
        stepPercent <- stepPercentageParser

        pure $ do
            algoId <- getAlgoId
            cfg <- StepInfoConfig queryMode algoId
                    <$> getPlatformId <*> getCommitId algoId <*> graphProps
                    <*> stepProps algoId <*> pure trainSeed <*> getDatasets
                    <*> pure shouldFilter <*> pure graphPercent
                    <*> pure variantPercent <*> pure stepPercent
                    <*> getUtcTime

            pure $ stepInfoQuery cfg
    ]
  where
    queryModeParser :: Parser QueryMode
    queryModeParser =
        optionParserFromValues modeMap "QUERY-MODE" helpTxt $ mconcat
            [ short 'q', long "query-mode", value StepQuery.All
            , showDefaultWith (map toLower . show)
            ]
      where
        helpTxt = "Query mode."
        modeMap = M.fromList
            [ ("train", StepQuery.Train)
            , ("validate", StepQuery.Validate)
            , ("all", StepQuery.All)
            ]

    graphProps :: SqlM (Set Text)
    graphProps = do
        graphPropQuery <- getDistinctFieldQuery GraphPropProperty
        runSqlQueryConduit graphPropQuery $ C.foldMap S.singleton

    stepProps :: Key Algorithm -> SqlM (Set Text)
    stepProps algoId = S.fromList . map (stepPropProperty . entityVal) <$>
        Sql.selectList [StepPropAlgorithmId ==. algoId] []

defaultImplParser :: Parser (Either Int Text)
defaultImplParser = implParser <|> pure (Right "edge-list")
  where
    implParser = option (Left <$> auto <|> Right <$> str) $ mconcat
        [ metavar "IMPLEMENTATION", short 'i', long "default-impl"
        , help "Default implementation in case of no valid prediction. \
               \Numeric or textual."
        ]

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

trainSeedParser :: Parser Int64
trainSeedParser = option auto . mconcat $
    [ metavar "N", short 's', long "seed", value 42, showDefault
    , help "Seed for training set randomisation" ]

stepInfoConfig :: Parser (SqlM StepInfoConfig)
stepInfoConfig = do
    getAlgoId <- algorithmIdParser
    getPlatformId <- platformIdParser
    getCommitId <- commitIdParser
    getUtcTime <- utcTimeParser
    trainSeed <- trainSeedParser
    getDatasets <- datasetsParser
    filterGraphProps <- props "graph"
    filterStepProps <- props "step"
    shouldFilter <- filterIncomplete
    graphPercent <- graphPercentageParser
    variantPercent <- variantPercentageParser
    stepPercent <- stepPercentageParser

    pure $ do
        algoId <- getAlgoId
        StepInfoConfig StepQuery.Train algoId
                <$> getPlatformId <*> getCommitId algoId
                <*> (filterGraphProps <*> gatherGraphProps)
                <*> (filterStepProps <*> gatherStepProps algoId)
                <*> pure trainSeed <*> getDatasets <*> pure shouldFilter
                <*> pure graphPercent <*> pure variantPercent
                <*> pure stepPercent <*> getUtcTime
  where
    readProps :: MonadIO m => FilePath -> m (Set Text)
    readProps = liftIO . fmap (S.fromList . T.lines) . T.readFile

    props :: MonadIO m => String -> Parser (m (Set Text -> Set Text))
    props name = asum [keepFilter name, dropFilter name, pure (return id)]

    keepFilter :: MonadIO m => String -> Parser (m (Set Text -> Set Text))
    keepFilter name = fmap keepProps <$> (readProps <$> keepOpt)
      where
        keepProps :: Set Text -> Set Text -> Set Text
        keepProps input db
            | S.null input = db
            | otherwise = S.intersection input db

        keepOpt = strOption $ mconcat
            [ metavar "FILE", long ("keep-" <> name <> "-props")
            , help "File listing properties to use for training, one per line."
            ]

    dropFilter :: MonadIO m => String -> Parser (m (Set Text -> Set Text))
    dropFilter name = fmap dropProps <$> (readProps <$> dropOpt)
      where
        dropProps :: Set Text -> Set Text -> Set Text
        dropProps input db
            | S.null input = db
            | otherwise = S.difference db input

        dropOpt = strOption $ mconcat
            [ metavar "FILE", long ("drop-" <> name <> "-props")
            , help "File listing properties not to use for training, \
                   \one per line."]

    gatherGraphProps :: SqlM (Set Text)
    gatherGraphProps = do
        query <- getDistinctFieldQuery GraphPropProperty
        runSqlQueryConduit query $ C.foldMap S.singleton

    gatherStepProps :: Key Algorithm -> SqlM (Set Text)
    gatherStepProps algoId = do
        stepProps <- Sql.selectList [StepPropAlgorithmId ==. algoId] []
        return . S.fromList $ map (stepPropProperty . entityVal) stepProps
