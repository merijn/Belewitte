{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module OptionParsers
    ( reflow
    , algorithmIdParser
    , algorithmParser
    , commitIdParser
    , datasetIdParser
    , datasetParser
    , entityParser
    , filterIncomplete
    , modelIdParser
    , modelParser
    , percentageParser
    , platformIdParser
    , platformParser
    , runconfigIdParser
    , runconfigParser
    , utcTimeParser
    , variantConfigIdParser
    , variantConfigParser
    , variantIdParser
    , variantParser
    , variantInfoConfigParser
    , intervalReader
    , readCI
    , mapOption
    , optionEnumerationHelp
    , optionParserFromValues
    , module Options.Applicative
    ) where

import Data.Char (toLower)
import Data.Function (on)
import Data.Interval (Extended(Finite), Interval)
import qualified Data.Interval as I
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Options.Applicative hiding (Completer)
import Options.Applicative.Help (Doc, (</>))
import qualified Options.Applicative.Help as Help
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Read (readMaybe)

import Core
import FieldQuery (getDistinctAlgorithmVersionQuery)
import Query (runSqlQuerySingle)
import Schema
import Sql (ToBackendKey, SqlBackend, (==.))
import qualified Sql
import VariantQuery (VariantInfoConfig(..))

reflow :: String -> Doc
reflow = Help.fillSep . map Help.text . words

algorithmIdParser :: Parser (SqlM (Key Algorithm))
algorithmIdParser = fmap entityKey <$> algorithmParser

algorithmParser :: Parser (SqlM (Entity Algorithm))
algorithmParser = queryAlgorithm <$> algorithmOpt
  where
    algorithmOpt :: Parser (Either Text Int64)
    algorithmOpt = option (Right <$> auto <|> Left <$> str) $ mconcat
        [ metavar "ID", short 'a', long "algorithm"
        , help "Algorithm to use, numeric or textual id" ]

    queryAlgorithm :: Either Text Int64 -> SqlM (Entity Algorithm)
    queryAlgorithm (Right n) = Sql.validateEntity "Algorithm" n
    queryAlgorithm (Left name) = Sql.validateUniqEntity "Algorithm" $
        UniqAlgorithm name

commitIdParser :: Parser (Key Algorithm -> SqlM CommitId)
commitIdParser = checkUniqueCommitId <$> commitIdOpt
  where
    commitIdOpt :: Parser (Maybe Text)
    commitIdOpt = optional . option str $ mconcat
        [ metavar "COMMIT", short 'c', long "commit"
        , help "Algorithm version to use" ]

    checkUniqueCommitId :: Maybe Text -> Key Algorithm -> SqlM CommitId
    checkUniqueCommitId txt algoId = do
        runSqlQuerySingle $ getDistinctAlgorithmVersionQuery algoId txt

datasetIdParser :: Parser (SqlM (Key Dataset))
datasetIdParser = fmap entityKey <$> datasetParser

datasetParser :: Parser (SqlM (Entity Dataset))
datasetParser = queryDataset <$> datasetOpt
  where
    datasetOpt :: Parser (Either Text Int64)
    datasetOpt = option (Right <$> auto <|> Left <$> str) $ mconcat
        [ metavar "ID", long "dataset"
        , help "Dataset results to use, numeric or textual" ]

    queryDataset :: Either Text Int64 -> SqlM (Entity Dataset)
    queryDataset (Right n) = Sql.validateEntity "Dataset" n
    queryDataset (Left name) = Sql.validateUniqEntity "Dataset" $
        UniqDataset name

entityParser :: ToBackendKey SqlBackend a => Parser (SqlM (Entity a))
entityParser = checkKey <$> keyParser
  where
    keyParser :: ToBackendKey SqlBackend v => Parser (Key v)
    keyParser = argument (toSqlKey <$> auto) $ mconcat
        [ metavar "ID", help "Id to display." ]

    checkKey :: ToBackendKey SqlBackend a => Key a -> SqlM (Entity a)
    checkKey key = do
        result <- Sql.getEntity key
        case result of
            Nothing -> logErrorN msg >> liftIO exitFailure
            Just v -> return v
      where
        msg :: Text
        msg = "No entity with id #" <> showSqlKey key

filterIncomplete :: Parser Bool
filterIncomplete = flag True False $ mconcat
    [ long "show-incomplete"
    , help "Include results for variants where some results are missing"
    ]

modelIdParser :: Parser (SqlM (Key PredictionModel))
modelIdParser = fmap entityKey <$> modelParser

modelParser :: Parser (SqlM (Entity PredictionModel))
modelParser = queryModel <$> modelOpt
  where
    modelOpt :: Parser Int64
    modelOpt = option auto $ mconcat
        [ metavar "ID", short 'm', long "model"
        , help "Model to use"
        ]

    queryModel :: Int64 -> SqlM (Entity PredictionModel)
    queryModel n = Sql.validateEntity "PredictionModel" n

percentageParser :: [Mod OptionFields Percentage] -> Parser Percentage
percentageParser opts = option (maybeReader percentReader) . mconcat $
    [ value ($$(validRational 1) :: Percentage)
    , showDefaultWith (show . getPercentage)
    ] ++ opts
  where
    percentReader :: String -> Maybe Percentage
    percentReader s = readMaybe s >>= mkPercentage

platformIdParser :: Parser (SqlM (Key Platform))
platformIdParser = fmap entityKey <$> platformParser

platformParser :: Parser (SqlM (Entity Platform))
platformParser = queryPlatform <$> platformOpt
  where
    platformOpt :: Parser (Either Text Int64)
    platformOpt = option (Right <$> auto <|> Left <$> str) $ mconcat
        [ metavar "ID", short 'p', long "platform"
        , help "Platform results to use, numeric or textual" ]

    queryPlatform :: Either Text Int64 -> SqlM (Entity Platform)
    queryPlatform (Right n) = Sql.validateEntity "Platform" n
    queryPlatform (Left name) = Sql.validateUniqEntity "platform" $
        UniqPlatform name

runconfigIdParser :: Parser (SqlM (Key RunConfig))
runconfigIdParser = fmap entityKey <$> runconfigParser

runconfigParser :: Parser (SqlM (Entity RunConfig))
runconfigParser = queryRunConfig <$> runConfigOpt
  where
    runConfigOpt :: Parser Int64
    runConfigOpt = option auto $ mconcat
        [ metavar "ID", short 'r', long "runconfig"
        , help "Numeric id of runconfig to use" ]

    queryRunConfig :: Int64 -> SqlM (Entity RunConfig)
    queryRunConfig n = Sql.validateEntity "RunConfig" n

utcTimeParser :: Parser (SqlM UTCTime)
utcTimeParser = maybe (liftIO getCurrentTime) return <$> optional timeParser
  where
    timeParser :: Parser UTCTime
    timeParser = option utcReader . mconcat $
        [ metavar "TIME", short 't', long "time"
        , help "Timestamp controlling query output." ]

    utcReader :: ReadM UTCTime
    utcReader = maybeReader $
        parseTimeM False defaultTimeLocale "%Y-%-m-%-d %T"

variantConfigIdParser :: Parser (Key Algorithm -> SqlM (Key VariantConfig))
variantConfigIdParser = fmap (fmap entityKey) <$> variantConfigParser

variantConfigParser :: Parser (Key Algorithm -> SqlM (Entity VariantConfig))
variantConfigParser = queryVariantConfig <$> variantConfigOpt
  where
    variantConfigOpt :: Parser (Maybe Int64)
    variantConfigOpt = option (maybeReader variantConfigReader) $ mconcat
        [ metavar "ID", long "variant", help "Numeric id of variant to use" ]

    variantConfigReader :: String -> Maybe (Maybe Int64)
    variantConfigReader s
        | "default" `isPrefixOf` s = Just Nothing
        | otherwise = Just <$> readMaybe s

    queryVariantConfig
        :: Maybe Int64 -> Key Algorithm -> SqlM (Entity VariantConfig)
    queryVariantConfig key algoId = case key of
        Just n -> Sql.validateEntity "VariantConfig" n
        Nothing -> Sql.selectSingle
                [ VariantConfigAlgorithmId ==. algoId
                , VariantConfigIsDefault ==. True
                ]

variantIdParser :: Parser (SqlM (Key Variant))
variantIdParser = fmap entityKey <$> variantParser

variantParser :: Parser (SqlM (Entity Variant))
variantParser = queryVariant <$> variantOpt
  where
    variantOpt :: Parser Int64
    variantOpt = option auto $ mconcat
        [ metavar "ID", long "variant", help "Numeric id of variant to use" ]

    queryVariant :: Int64 -> SqlM (Entity Variant)
    queryVariant n = Sql.validateEntity "Variant" n

variantInfoConfigParser :: Parser (SqlM VariantInfoConfig)
variantInfoConfigParser = do
    getAlgoId <- algorithmIdParser
    getCommit <- commitIdParser
    getPlatformId <- platformIdParser
    getDatasetId <- optional datasetIdParser
    filterFlag <- filterIncomplete

    pure $ do
        algoId <- getAlgoId
        VariantInfoConfig algoId
            <$> getPlatformId <*> getCommit algoId <*> pure Nothing
            <*> sequence getDatasetId <*> pure filterFlag

intervalReader :: ReadM (IntervalSet Int64)
intervalReader = maybeReader . parseMaybe $
    IS.fromList <$> sepBy1 interval (char ',')
  where
    interval :: Parsec () String (Interval Int64)
    interval = range <|> singleValue

    singleValue = I.singleton <$> decimal
    range = toInterval <$> try (decimal <* char '-') <*> decimal

    toInterval = I.interval `on` (,True) . Finite

readCI :: (Foldable f, Show a) => f a -> ReadM a
readCI vals = maybeReader lookupCI
  where
    lookupCI key = M.lookup (map toLower key) valMap
    valMap = foldMap (\v -> M.singleton (map toLower (show v)) v) vals

mapOption :: Map String v -> Mod OptionFields v -> Parser v
mapOption vals = option (maybeReader lookupCI)
  where
    lookupCI key = M.lookup (map toLower key) valMap
    valMap = M.mapKeys (map toLower) vals

optionEnumerationHelp :: String -> [String] -> Doc
optionEnumerationHelp metaVar names = preamble </> quoteValues names
  where
    preamble :: Doc
    preamble = reflow $ "The possible values of " <> metaVar <> " are"

    quote :: String -> Doc
    quote = Help.squotes . Help.string

    quoteValues :: [String] -> Doc
    quoteValues l = case l of
        [] -> mempty
        [x] -> Help.string "or" </> quote x <> Help.char '.' <> Help.softbreak
        (x:xs) -> quote x <> Help.char ',' </> quoteValues xs

optionParserFromValues
    :: Map String a -> String -> Doc -> Mod OptionFields a -> Parser a
optionParserFromValues vals metaVar helpText = mapOption vals . mappend extra
  where
    extra = mconcat [ metavar metaVar, helpDoc (Just extraHelpText) ]
    extraHelpText = helpText </> optionEnumerationHelp metaVar names
    names = map (map toLower) $ M.keys vals
