{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module ModelOptions
    ( ModelCommand(..)
    , PredictorExport(..)
    , commands
    , runCommand
    ) where

import Control.Monad (forM)
import Data.Bifunctor (second)
import Data.Char (toLower)
import qualified Data.Conduit.Combinators as C
import Data.Foldable (asum)
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Any(..))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Core
import Evaluate
    ( CompareReport
    , EvaluateReport
    , Report(..)
    , Splittable(..)
    , ReportOutput(..)
    , RelativeTo(..)
    , SortBy(..)
    )
import Options
import Predictor (PredictorConfig)
import Pretty.List (Field(..), FieldSpec(..), (=.), buildOptions)
import Schema
import Sql ((==.))
import qualified Sql
import Query.Dump (modelQueryDump)
import Query.ImplRank (Column, Ranking, implRankQuery)
import qualified Query.ImplRank as ImplRank
import Query.Step (stepInfoQuery)
import Query.Train
    (QueryMode, StepInfoConfig(..), TrainStepConfig(..), trainStepQuery)
import qualified Query.Train as Train
import Query.Variant

data PredictorExport = CppFile FilePath | SharedLib FilePath

data ModelCommand
    = Train
      { getConfig :: SqlM TrainStepConfig }
    | QueryModel
      { getModelEntity :: SqlM (Entity PredictionModel) }
    | ListModels
      { listModels :: SqlM () }
    | ValidateModel
      { getPredictorConfig :: SqlM PredictorConfig
      , getOptionalPlatformId :: SqlM (Maybe (Key Platform))
      , getOptionalDatasetIds :: SqlM (Maybe (Set (Key Dataset)))
      }
    | EvaluatePredictor
      { getPlatformId :: SqlM (Key Platform)
      , getPredictorConfigs :: SqlM [PredictorConfig]
      , shouldFilterIncomplete :: Bool
      , evaluateConfig :: EvaluateReport
      , getDatasetIds :: SqlM (Maybe (Set (Key Dataset)))
      }
    | Compare
      { getVariantInfoConfig :: SqlM VariantInfoConfig
      , compareConfig :: CompareReport
      }
    | ExportPredictor
      { getPredictorConfig :: SqlM PredictorConfig
      , exportOutput :: PredictorExport
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
        } (Train <$> (trainStepConfig <*> pure Train.Train))
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
            <$> predictorConfigParser
            <*> (sequence <$> optional platformIdParser) <*> datasetsParser
    , SingleCommand CommandInfo
        { commandName = "evaluate"
        , commandHeaderDesc = "evaluate model performance"
        , commandDesc =
            "Evaluate BDT model performance on full dataset and compare \
            \against performance of other implementations"
        }
        $ EvaluatePredictor
            <$> platformIdParser <*> predictorConfigsParser
            <*> filterIncomplete <*> evaluateParser <*> datasetsParser
    , SingleCommand CommandInfo
        { commandName = "compare"
        , commandHeaderDesc = "compare implementation performance"
        , commandDesc = "Compare the performance of different implementations"
        }
        $ Compare <$> variantInfoConfigParser <*> compareParser
    , SingleCommand CommandInfo
        { commandName = "export"
        , commandHeaderDesc = "export model"
        , commandDesc = "Export BDT model"
        } (ExportPredictor <$> predictorConfigParser <*> (SharedLib <$> soFile))
    , SingleCommand CommandInfo
        { commandName = "export-source"
        , commandHeaderDesc = "export model C++ source"
        , commandDesc = "Export BDT model to C++ source"
        } (ExportPredictor <$> predictorConfigParser <*> (CppFile <$> cppFile))
    ]
  }
  where
    soFile :: Parser FilePath
    soFile = strArgument . mconcat $
        [ metavar "FILE", value "model.so"
        , showDefaultWith id, help "Shared library file to create." ]

    cppFile :: Parser FilePath
    cppFile = strArgument . mconcat $
        [ metavar "FILE", value "model.cpp"
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
    ]

modelQueryMap :: Map String (Parser DebugQuery)
modelQueryMap = M.fromList
    [ nameDebugQuery "trainStepQuery" $
        fmap Train.sortStepTimings . trainStepQuery <$> Compose (trainStepConfig <*> queryModeParser)
    , nameDebugQuery "stepInfoQuery" $
        fmap (uncurry stepInfoQuery) . Compose $ do
            getAlgoId <- algorithmIdParser
            getStepInfoConfig <- stepInfoConfig
            getVariantId <- variantIdParser
            pure $ do
                algoId <- getAlgoId
                (,) <$> getStepInfoConfig algoId <*> getVariantId algoId

    , nameDebugQuery "implRankQuery" $
        implRankQuery <$> fullStepInfoConfig <*> columnParser <*> rankParser
    ]
  where
    fullStepInfoConfig :: Compose Parser SqlM StepInfoConfig
    fullStepInfoConfig = Compose $ do
        getAlgoId <- algorithmIdParser
        getStepInfoConfig <- stepInfoConfig
        pure $ getAlgoId >>= getStepInfoConfig

    queryModeParser :: Parser QueryMode
    queryModeParser =
        optionParserFromValues modeMap "QUERY-MODE" helpTxt $ mconcat
            [ short 'q', long "query-mode", value Train.All
            , showDefaultWith (map toLower . show)
            ]
      where
        helpTxt = "Query mode."
        modeMap = M.fromList
            [ ("train", Train.Train)
            , ("validate", Train.Validate)
            , ("all", Train.All)
            ]

    columnParser :: Compose Parser SqlM Column
    columnParser = Compose . fmap pure .
        optionParserFromValues modeMap "COLUMN" helpTxt $ mconcat
            [ long "column", value ImplRank.AvgTime
            , showDefaultWith (const "avg")
            ]
      where
        helpTxt = "The timing data to rank by."
        modeMap = M.fromList
            [ ("min", ImplRank.MinTime)
            , ("avg", ImplRank.AvgTime)
            , ("max", ImplRank.MaxTime)
            ]

    rankParser :: Compose Parser SqlM Ranking
    rankParser = Compose . fmap pure .
        optionParserFromValues modeMap "RANKING" helpTxt $ mconcat
            [ long "ranking", value ImplRank.Avg
            , showDefaultWith (const "avg")
            ]
      where
        helpTxt = "How to rank implementation timings."
        modeMap = M.fromList
            [ ("min", ImplRank.Min)
            , ("avg", ImplRank.Avg)
            , ("total", ImplRank.Total)
            ]

reportParser :: Map String RelativeTo -> Parser a -> Parser (Report a)
reportParser relTo implTypes =
  Report <$> variantIntervals <*> resultsRelativeTo <*> sortResultsBy
         <*> implTypes <*> (latexTable <|> detailed <|> pure Minimal)
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

    latexTable :: Parser ReportOutput
    latexTable = LaTeX <$> tableLabel <*> splittable
      where
        tableLabel = strOption $ mconcat
            [ metavar "LABEL", long "latex"
            , help "Show output as LaTeX table, using LABEL for the caption."
            ]

        splittable = flag Fixed Splittable $ mconcat
            [ long "splittable"
            , help "Allows LaTeX table to be split across multiple pages."
            ]

    detailed :: Parser ReportOutput
    detailed = flag' Detailed $ mconcat
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

filterImpls :: Set ImplType -> IntMap Implementation -> IntMap Implementation
filterImpls implTypes = IM.filter (implFilter . implementationType)
  where
    implFilter :: ImplType -> Bool
    implFilter = getAny . foldMap (\i -> Any . (==i)) implTypes

evaluateParser :: Parser EvaluateReport
evaluateParser = reportParser relToValues $ byImpls <|> byImplType
  where
    relToValues = M.insert "predicted" Predicted defaultRelativeToValues

    byImplType :: Parser (SqlM (IntMap Implementation -> IntMap Implementation))
    byImplType = return . filterImpls <$> implTypesParser S.singleton M.empty

    byImpls :: Parser (SqlM (IntMap Implementation -> IntMap Implementation))
    byImpls = readText <$> implementationFile <*> bestNonSwitching
      where
        implementationFile = strOption $ mconcat
            [ metavar "FILE", long "implementations"
            , help "File to read implementations to evaluate from"
            ]

        bestNonSwitching = flag True False $ mconcat
            [ long "drop-best-non-switching"
            , help "Drop the best non-switching implementation from the results."
            ]

    readText
        :: MonadIO m
        => FilePath -> Bool -> m (IntMap Implementation -> IntMap Implementation)
    readText path bestNonSwitching = liftIO $ toFilter <$> T.readFile path
      where
        toFilter = filterImplementations . S.fromList . T.lines

        filterImplementations
            :: Set Text -> IntMap Implementation -> IntMap Implementation
        filterImplementations textSet = dropBestNonSwitching . setFilter
          where
            setFilter = IM.filter $ getAny . mconcat
                [ Any . (`S.member` textSet) . implementationName
                , Any . (Builtin==) . implementationType
                ]

            dropBestNonSwitching
                | bestNonSwitching = id
                | otherwise = IM.delete bestNonSwitchingImplId

compareParser :: Parser CompareReport
compareParser = reportParser defaultRelativeToValues (second filterImpls <$> implTypes)
  where
    implTypes = implTypesParser ((mempty,) . S.singleton) extraVals
    extraVals = M.singleton "comparison" (Any True, S.empty)

trainSeedParser :: Parser Int64
trainSeedParser = option auto . mconcat $
    [ metavar "N", short 's', long "seed", value 42, showDefault
    , help "Seed for training set randomisation" ]

stepInfoConfig :: Parser (Key Algorithm -> SqlM StepInfoConfig)
stepInfoConfig = do
    getPlatformId <- platformIdParser
    getCommitId <- commitIdParser
    getUtcTime <- utcTimeParser
    shouldFilter <- filterIncomplete

    pure $ \algoId ->
        StepInfoConfig algoId
            <$> getPlatformId <*> getCommitId algoId <*> pure shouldFilter
            <*> getUtcTime

trainStepConfig :: Parser (QueryMode -> SqlM TrainStepConfig)
trainStepConfig = do
    getAlgoId <- algorithmIdParser
    getStepInfoConfig <- stepInfoConfig
    getDatasets <- datasetsParser
    trainSeed <- trainSeedParser
    filterGraphProps <- props "graph"
    filterStepProps <- props "step"
    graphPercent <- graphPercentageParser
    variantPercent <- variantPercentageParser
    stepPercent <- stepPercentageParser

    pure $ \queryMode -> do
        algoId <- getAlgoId
        stepConfig <- getStepInfoConfig algoId
        datasets <- getDatasets

        stepProps <- S.union
            <$> (filterGraphProps <*> getGraphProps)
            <*> (filterStepProps <*> getStepProps algoId)

        return $ TrainStepConfig
            { trainStepInfoConfig = stepConfig
            , trainStepQueryMode = queryMode
            , trainStepDatasets = datasets
            , trainStepProps = stepProps
            , trainStepSeed = trainSeed
            , trainStepGraphs = graphPercent
            , trainStepVariants = variantPercent
            , trainStepSteps = stepPercent
            }
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
