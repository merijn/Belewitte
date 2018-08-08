{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Options (ModelCommand(..), Options(..), optionsParser) where

import Control.Monad.Catch (throwM)
import Data.Char (toLower)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Functor.Compose (Compose(..))
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Persist.Sqlite as Sql

import Evaluate (Report(..), RelativeTo(..), SortBy(..))
import Model (Model, byteStringToModel)
import OptionParsers
import Schema
import Train (TrainingConfig(..))

data Options =
  Options
  { database :: Text
  , logVerbosity :: LogSource -> LogLevel -> Bool
  , modelTask :: ModelCommand
  }

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

optionsParser :: String -> IO Options
optionsParser name = execParser . info (options <**> helper) $ mconcat
    [ fullDesc
    , header $ name ++ " - a tool for generating and validating BDT models"
    , progDesc "Generate, validate, evaluate, and export Binary Decision Tree \
               \(BDT) models for predicing which GPU implementation to use \
               \for a GPU algorithm."
    ]
  where
    options :: Parser Options
    options = Options <$> databaseOption <*> verbosityOption <*> commands name

data ModelCommand
    = Train
      { getGpuId :: SqlM (Key GPU) , getConfig :: SqlM TrainingConfig }
    | Query
      { getModel :: SqlM (Key PredictionModel, Model) }
    | Validate
      { getGpuId :: SqlM (Key GPU)
      , getModel :: SqlM (Key PredictionModel, Model)
      }
    | Evaluate
      { getGpuId :: SqlM (Key GPU)
      , getModel :: SqlM (Key PredictionModel, Model)
      , reportConfig :: Report
      }
    | Compare
      { getGpuId :: SqlM (Key GPU), reportConfig :: Report }
    | Export
      { getModel :: SqlM (Key PredictionModel, Model), cppFile :: FilePath}

commands :: String -> Parser ModelCommand
commands name = hsubparser $ mconcat
    [ subCommand "train" "train a model"
        "Train a new model" $ Train <$> gpuParser <*> trainingConfig
    , subCommand "query" "report model info"
        "Report model info & statistics" $ Query <$> modelParser
    , subCommand "validate" "validate model accuracy"
        "Compute and report a model's accuracy on validation dataset and full \
        \dataset" $ Validate <$> gpuParser <*> modelParser
    , subCommand "evaluate" "evaluate model performance"
        "Evaluate BDT model performance on full dataset and compare against \
        \performance of other implementations" $
        Evaluate <$> gpuParser <*> modelParser <*> reportParser False
    , subCommand "compare" "compare implementation performance"
        "Compare the performance of different implementations" $
        Compare <$> gpuParser <*> reportParser True
    , subCommand "export" "export model to C++"
        "Export BDT model to C++ file" $ Export <$> modelParser <*> cppFile
    ]
  where
    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ name ++ " " ++ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

    cppFile = strOption . mconcat $
        [ metavar "FILE", short 'e', long "export", value "test.cpp"
        , showDefaultWith id, help "C++ file to write predictor to." ]

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
        return $ (key, byteStringToModel predictionModelModel)
      where
        key = toSqlKey n

gpuParser :: Parser (SqlM (Key GPU))
gpuParser = queryGPU <$> gpuOpt
  where
    gpuOpt :: Parser (Either Text Int64)
    gpuOpt = option (Right <$> auto <|> Left <$> auto) $ mconcat
        [ metavar "ID", short 'g', long "gpu"
        , help "GPU results to use, numeric or textual" ]

    queryGPU :: Either Text Int64 -> SqlM (Key GPU)
    queryGPU (Right n) = validateKey n
    queryGPU (Left name) = do
        Just Sql.Entity{entityKey} <-
            logIfFail "No GPU with name" name . Sql.getBy $ UniqGPU name
        return entityKey

reportParser :: Bool -> Parser Report
reportParser isComparison =
  Report <$> variantIntervals <*> resultsRelativeTo <*> sortResultsBy
         <*> implTypes <*> pure False
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
    resultsRelativeTo = optionParserFromValues values $ mconcat
        --FIXME: list options
        [ metavar "REL-TO", long "rel-to", value Optimal
        , showDefaultWith (map toLower . show)
        , help "Results to normalise result output to" ]
      where
        base = M.fromList [("optimal", Optimal), ("best", BestNonSwitching)]

        values
          | isComparison = base
          | otherwise = M.insert "predicted" Predicted $ base

    sortResultsBy :: Parser SortBy
    sortResultsBy = optionParserFromValues values $ mconcat
        --FIXME: list options
        [ metavar "SORT-BY", long "sort-by", value Avg
        , showDefaultWith (map toLower . show)
        , help "Sort results by" ]
      where
        values = M.fromList [("avg", Avg), ("max", Max)]

    implTypes :: Parser (Set ImplType)
    implTypes = S.unions <$> many implParser <|> pure (S.singleton Core)
      where
        implParser = optionParserFromValues values $ mconcat
            --FIXME: list options
            [ metavar "TYPE", long "impl-type"
            , showDefault
            , help "Implementation types to output results for" ]

        values = M.fromList
            [ ("core", S.singleton Core)
            , ("derived", S.singleton Derived)
            , ("comparison", S.singleton Comparison)
            ]

type SqlParser = Compose Parser SqlM

trainingConfig :: Parser (SqlM TrainingConfig)
trainingConfig = getCompose $
    TrainConfig <$> props "graph" <*> props "step" <*> trainFract <*> seedOpt
  where
    seedOpt = Compose . fmap pure . option auto . mconcat $
        [ metavar "N", short 's', long "seed", value 42, showDefault
        , help "Seed for training set randomisation" ]

    trainFract = Compose . fmap pure . option auto . mconcat $
        [ metavar "PERCENT", short 'p', long "percent", value 0.8, showDefault
        , help "Training set as percentage of data." ]

    props :: Text -> SqlParser (Set Text)
    props name = keepFilter name <|> dropFilter name <|> gatherProps name

    keepFilter :: Text -> SqlParser (Set Text)
    keepFilter name = keepProps <$> gatherProps name
                                <*> Compose (readProps <$> keepOpt)
      where
        keepOpt = strOption $ mconcat
            [ metavar "FILE", long ("keep-" <> T.unpack name <> "-props")
            , help "File listing properties to use for training, one per line."
            ]

    dropFilter :: Text -> SqlParser (Set Text)
    dropFilter name = dropProps <$> gatherProps name
                                <*> Compose (readProps <$> dropOpt)
      where
        dropOpt = strOption $ mconcat
            [ metavar "FILE", long ("drop-" <> T.unpack name <> "-props")
            , help "File listing properties not to use for training, \
                   \one per line."]

    gatherProps :: Text -> SqlParser (Set Text)
    gatherProps name = Compose . pure . runConduit $
        Sql.rawQuery query [] .| C.foldMapM toSet
      where
        toSet [Sql.PersistText t] = return $ S.singleton t
        toSet _ = throwM . Error $ "Unexpected result from property query"

        query = [i|SELECT DISTINCT property FROM #{T.toTitle name}Prop|]

