{-# LANGUAGE TupleSections #-}
module Options (Options(..), optionsParser) where

import Data.Function (on)
import Data.Interval (Extended(Finite), Interval)
import qualified Data.Interval as I
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

import Analyse (Report(..), RelativeTo(..), SortBy(..))
import OptionParsers
import Schema

data Options =
  Options
  { database :: Text
  , logVerbosity :: LogSource -> LogLevel -> Bool
  , seed :: Int
  , trainingFraction :: Double
  , exportFile :: Maybe String
  , selectProps :: IO (Set Text -> Set Text)
  , dumpFile :: Maybe FilePath
  , loadFile :: Maybe FilePath
  , doValidate :: Bool
  , analysisReporting :: Maybe Report
  }

readProps :: FilePath -> IO (Set Text)
readProps = fmap (S.fromList . T.lines) . T.readFile

keepProps :: Set Text -> Set Text -> Set Text
keepProps input db
    | S.null input = db
    | otherwise = S.intersection input db

dropProps :: Set Text -> Set Text -> Set Text
dropProps input db
    | S.null input = db
    | otherwise = S.difference db input

optionsParser :: String -> IO Options
optionsParser name = execParser . info (commands <**> helper) $ mconcat
    [ fullDesc
    , header $ name ++ " - a tool for generating and validating BDT models"
    , progDesc "Generate, validate, evaluate, and export Binary Decision Tree \
               \(BDT) models for predicing which GPU implementation to use \
               \for a GPU algorithm."
    ]

commands :: Parser Options
commands = Options <$> databaseOption <*> verbosityOption <*> seedOpt
                   <*> trainFract <*> (cppFile <|> pure Nothing) <*> props
                   <*> dump <*> load <*> validate <*> reporting
  where
    seedOpt = option auto . mconcat $
        [ metavar "N", short 's', long "seed", value 42, showDefault
        , help "Seed for training set randomisation." ]

    trainFract = option auto . mconcat $
        [ metavar "PERCENT", short 'p', long "percent", value 0.8, showDefault
        , help "Training set as percentage of data." ]

    cppFile = fmap Just . strOption . mconcat $
        [ metavar "FILE", short 'e', long "export", value "test.cpp"
        , showDefaultWith id, help "C++ file to write predictor to." ]

    dump = Just <$> dumpOpt <|> pure Nothing
      where
        dumpOpt = strOption $ mconcat
            [ metavar "FILE", long "dump"
            , help "File to dump raw model to." ]

    load = Just <$> loadOpt <|> pure Nothing
      where
        loadOpt = strOption $ mconcat
            [ metavar "FILE", long "load"
            , help "File to read raw model from." ]

    props = keepFilter <|> dropFilter <|> keepAll

    keepFilter = fmap keepProps . readProps <$> keepOpt
      where
        keepOpt = strOption $ mconcat
            [ metavar "FILE", long "keep"
            , help "File listing properties to use for training, one per line."
            ]

    dropFilter = fmap dropProps . readProps <$> dropOpt
      where
        dropOpt = strOption $ mconcat
            [ metavar "FILE", long "drop"
            , help "File listing properties not to use for training, \
                   \one per line."]

    keepAll = pure (return id)

    validate :: Parser Bool
    validate = flag False True $ mconcat
        [ long "validate", help "Run model validation." ]

    reporting :: Parser (Maybe Report)
    reporting  = (analyse <*> report) <|> pure Nothing

    analyse :: Parser (a -> Maybe a)
    analyse = flag' Just $ mconcat
        [ long "analyse", help "Run model analysis." ]

    report :: Parser Report
    report = Report <$> variantIntervals <*> resultsRelativeTo
                    <*> sortResultsBy <*> implTypes

    variantIntervals :: Parser (IntervalSet Int64)
    variantIntervals = IS.unions <$> many intervals
      where
        intervals = intervalFlagRange <|> defaultIntervalFlag

    defaultIntervalFlag :: Parser (IntervalSet Int64)
    defaultIntervalFlag = flag' IS.whole $ mconcat
        [ long "report-all", help "Reports results for all variants." ]

    intervalFlagRange :: Parser (IntervalSet Int64)
    intervalFlagRange = option intervals $ mconcat
        [ metavar "RANGE", long "report-range"
        , help "Range(s) of variants to print results for. Accepts \
               \comma-seperated ranges. A range is dash-separated inclusive \
               \range or a single number. Example: \
               \--report-range=5-10,13,17-20" ]
      where
        intervals :: ReadM (IntervalSet Int64)
        intervals = maybeReader . parseMaybe $
            IS.fromList <$> sepBy1 interval (char ',')
          where
            interval :: Parsec () String (Interval Int64)
            interval = range <|> singleValue

            singleValue = I.singleton <$> decimal
            range = toInterval <$> try (decimal <* char '-') <*> decimal

            toInterval = I.interval `on` (,True) . Finite

    resultsRelativeTo :: Parser RelativeTo
    resultsRelativeTo = pure Optimal

    sortResultsBy :: Parser SortBy
    sortResultsBy = pure Avg

    implTypes :: Parser (Set ImplType)
    implTypes = pure $ S.singleton Core
