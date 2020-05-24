{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module ModelOptions (ModelCommand(..), commands, runCommand) where

import Control.Monad (forM)
import Data.Char (toLower)
import qualified Data.Conduit.Combinators as C
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
import Options
import Pretty.List (Field(..), FieldSpec(..), (=.), buildOptions)
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
      , shouldFilterIncomplete :: Bool
      , evaluateConfig :: EvaluateReport
      , getDatasetIds :: SqlM (Maybe (Set (Key Dataset)))
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
  , mainCommands = SubCommands
    [ SingleCommand CommandInfo
        { commandName = "train"
        , commandHeaderDesc = "train a model"
        , commandDesc = "Train a new model"
        } (Train <$> (stepInfoConfig <*> pure StepQuery.Train))
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
            <*> datasetsParser
    , SingleCommand CommandInfo
        { commandName = "evaluate"
        , commandHeaderDesc = "evaluate model performance"
        , commandDesc =
            "Evaluate BDT model performance on full dataset and compare \
            \against performance of other implementations"
        }
        $ EvaluatePredictor
            <$> platformIdParser <*> modelIdParser <*> defaultImplParser
            <*> filterIncomplete <*> evaluateParser <*> datasetsParser
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

getGraphProps :: SqlM (Map Text (Key PropertyName))
getGraphProps = Sql.selectSource [PropertyNameIsStepProp ==. False] [] $
    C.foldMap propMap
  where
    propMap (Entity key PropertyName{propertyNameProperty}) =
        M.singleton propertyNameProperty key

getStepProps :: Key Algorithm -> SqlM (Map Text (Key PropertyName))
getStepProps algoId = do
    stepPropIds <- Sql.selectList [StepPropAlgorithmId ==. algoId] []
    stepProps <- forM stepPropIds $ \(Entity _ prop) ->
        Sql.getJustEntity (stepPropPropId prop)

    return $ foldMap propMap stepProps
  where
    propMap (Entity key PropertyName{propertyNameProperty}) =
        M.singleton propertyNameProperty key

datasetsParser :: Parser (SqlM (Maybe (Set (Key Dataset))))
datasetsParser = sequence <$> optional rawDatasetsParser
  where
    rawDatasetsParser :: Parser (SqlM (Set (Key Dataset)))
    rawDatasetsParser = fmap S.fromList . sequence <$> some datasetIdParser

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
    [ nameDebugQuery "stepInfoQuery" $
        fmap StepQuery.sortStepTimings . stepInfoQuery <$> Compose (stepInfoConfig <*> queryModeParser)
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

stepInfoConfig :: Parser (QueryMode -> SqlM StepInfoConfig)
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

    pure $ \queryMode -> do
        algoId <- getAlgoId
        stepProps <- S.union <$> (filterGraphProps <*> getGraphProps)
                             <*> (filterStepProps <*> getStepProps algoId)
        StepInfoConfig queryMode algoId
                <$> getPlatformId <*> getCommitId algoId
                <*> pure stepProps
                <*> pure trainSeed <*> getDatasets <*> pure shouldFilter
                <*> pure graphPercent <*> pure variantPercent
                <*> pure stepPercent <*> getUtcTime
  where
    readProps :: MonadIO m => FilePath -> m (Set Text)
    readProps = liftIO . fmap (S.fromList . T.lines) . T.readFile

    props
        :: MonadIO m
        => String
        -> Parser (m (Map Text (Key PropertyName) -> Set (Key PropertyName)))
    props name = asum
        [ keepFilter name
        , dropFilter name
        , pure . return $ S.fromList . M.elems
        ]

    keepFilter
        :: MonadIO m
        => String
        -> Parser (m (Map Text (Key PropertyName) -> Set (Key PropertyName)))
    keepFilter name = fmap keepProps <$> (readProps <$> keepOpt)
      where
        keepProps
            :: Set Text
            -> Map Text (Key PropertyName)
            -> Set (Key PropertyName)
        keepProps input db
            | S.null input = S.fromList $ M.elems db
            | otherwise = S.fromList . M.elems $
                M.filterWithKey (\key _ -> S.member key input) db

        keepOpt = strOption $ mconcat
            [ metavar "FILE", long ("keep-" <> name <> "-props")
            , help "File listing properties to use for training, one per line."
            ]

    dropFilter
        :: MonadIO m
        => String
        -> Parser (m (Map Text (Key PropertyName) -> Set (Key PropertyName)))
    dropFilter name = fmap dropProps <$> (readProps <$> dropOpt)
      where
        dropProps
            :: Set Text
            -> Map Text (Key PropertyName)
            -> Set (Key PropertyName)
        dropProps input db
            | S.null input = S.fromList $ M.elems db
            | otherwise = S.fromList . M.elems $ M.withoutKeys db input

        dropOpt = strOption $ mconcat
            [ metavar "FILE", long ("drop-" <> name <> "-props")
            , help "File listing properties not to use for training, \
                   \one per line."]
