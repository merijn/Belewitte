{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Options (ModelCommand(..), commands, runSqlM) where

import Data.Char (isSpace, toLower)
import qualified Data.Conduit.Combinators as C
import Data.Functor.Compose (Compose(..))
import Data.Foldable (asum)
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Core
import Evaluate (Report(..), RelativeTo(..), SortBy(..))
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
      , reportConfig :: Report
      }
    | Compare
      { getAlgoId :: SqlM (Key Algorithm)
      , getPlatformId :: SqlM (Key Platform)
      , reportConfig :: Report
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

commands :: String -> (InfoMod a, Parser ModelCommand)
commands name = (,) docs . (<|>) hiddenCommands . hsubparser $ mconcat
    [ subCommand "train" "train a model" "Train a new model" $
        Train <$> trainingConfig
    , subCommand "query" "report model info"
        "Report model info & statistics" $ QueryModel <$> modelParser
    , subCommand "validate" "validate model accuracy"
        "Compute and report a model's accuracy on validation dataset and full \
        \dataset" $ Validate <$> algorithmIdParser <*> platformIdParser
                             <*> modelParser
    , subCommand "evaluate" "evaluate model performance"
        "Evaluate BDT model performance on full dataset and compare against \
        \performance of other implementations" $
        Evaluate <$> algorithmParser <*> platformIdParser <*> modelParser
                 <*> defaultImplParser <*> reportParser False
    , subCommand "compare" "compare implementation performance"
        "Compare the performance of different implementations" $
        Compare <$> algorithmIdParser <*> platformIdParser
                <*> reportParser True
    , subCommand "export" "export model to C++"
        "Export BDT model to C++ file" $
        Export <$> algorithmIdParser <*> modelParser <*> cppFile
    ]
  where
    docs :: InfoMod a
    docs = mconcat
      [ fullDesc
      , header $ name ++ " - a tool for generating and validating BDT models"
      , progDesc
        "Generate, validate, evaluate, and export Binary Decision Tree (BDT) \
        \models for predicting which implementation to use for an algorithm."
      ]

    subCommand :: String-> String -> String -> Parser a -> Mod CommandFields a
    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ name ++ " " ++ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

    cppFile :: Parser FilePath
    cppFile = strOption . mconcat $
        [ metavar "FILE", short 'e', long "export", value "test.cpp"
        , showDefaultWith id, help "C++ file to write predictor to." ]

    hiddenCommands :: Parser ModelCommand
    hiddenCommands = hsubparser . mappend internal $
        subCommand "query-test" "check query output"
            "Dump query output to files to validate results" $
            QueryTest <$> algorithmIdParser <*> platformIdParser
                      <*> (suffixParser <|> pure Nothing)

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
    implTypes = S.insert Builtin <$>
        (S.unions <$> some implParser <|> pure (S.singleton Core))
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
