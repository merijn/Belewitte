{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module OptionParsers
    ( reflow
    , algorithmIdParser
    , algorithmParser
    , allowNewerParser
    , commitIdParser
    , datasetIdParser
    , datasetParser
    , entityParser
    , filterIncomplete
    , intMapFilter
    , intervalFlag
    , modelIdParser
    , modelParser
    , percentageParser
    , platformIdParser
    , platformParser
    , predictorConfigParser
    , predictorConfigsParser
    , renameParser
    , runconfigIdParser
    , runconfigParser
    , requiredUtcTimeParser
    , setParser
    , utcTimeParser
    , variantConfigIdParser
    , variantConfigParser
    , variantIdParser
    , variantParser
    , variantInfoConfigParser
    , readCI
    , mapOption
    , optionEnumerationHelp
    , optionParserFromValues
    , module Options.Applicative
    ) where

import Control.Monad (void, when)
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.Function (on)
import Data.Interval (Extended(Finite), Interval)
import qualified Data.Interval as I
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.List (isPrefixOf)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Void (Void)
import Options.Applicative hiding (Completer)
import Options.Applicative.Help (Doc, (</>))
import qualified Options.Applicative.Help as Help
import System.Exit (exitFailure)
import Text.Megaparsec
    ( Parsec, eof, lookAhead, manyTill, parseMaybe, runParser, sepBy1, takeRest
    , try)
import Text.Megaparsec.Char (alphaNumChar, char, string')
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Read (readMaybe)

import Core
import Predictor.Config
    (PredictorConfig, MispredictionStrategy(..), mkPredictorConfig)
import Query (runSqlQuerySingle)
import Query.Field (getDistinctAlgorithmVersionQuery)
import Query.ImplRank (Column(..), Ranking(..))
import Query.Variant (VariantInfoConfig(..))
import Schema
import Sql (ToBackendKey, SqlBackend, (==.))
import qualified Sql

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
    queryAlgorithm (Right n) = Sql.validateEntity n
    queryAlgorithm (Left name) = Sql.validateUniqEntity $ UniqAlgorithm name

allowNewerParser :: Parser AllowNewer
allowNewerParser = optionParserFromValues vals "OPT" helpTxt $ mconcat
        [ long "allow-newer", value NoNewer, showDefaultWith (const "no") ]
  where
    helpTxt = "Whether to include results newer than specified timestamp. "
    vals = M.fromList
        [ ("no", NoNewer)
        , ("results", NewerResults)
        , ("impls", NewerImpls)
        , ("all", AllNewer)
        ]

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
    queryDataset (Right n) = Sql.validateEntity n
    queryDataset (Left name) = Sql.validateUniqEntity $ UniqDataset name

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

intMapFilter :: String -> String -> Parser (IntMap a -> IntMap a)
intMapFilter flagName name = filters
  where
    filters :: Parser (IntMap a -> IntMap a)
    filters = fmap mkFilter $ intervalFlag flagName name
      where
        mkFilter :: IntervalSet Int -> IntMap a -> IntMap a
        mkFilter intervals = IM.filterWithKey (\k _ -> k `IS.member` intervals)

intervalFlag :: Integral a => String -> String -> Parser (IntervalSet a)
intervalFlag flagName name = option intervalReader $ mconcat
    [ metavar "ID(s)", long flagName
    , help $ "Range(s) of " <> name <> " ids to print results for. Accepts \
        \comma-seperated ranges. A range is dash-separated inclusive \
        \range or a single number. Example: \
        \--impl-set=5-10,13,17-20"
    ]
  where
    intervalReader :: Integral a => ReadM (IntervalSet a)
    intervalReader = maybeReader . parseMaybe $
        IS.fromList <$> sepBy1 interval (char ',')
      where
        interval :: Integral a => Parsec () String (Interval a)
        interval = range <|> singleValue

        singleValue :: Integral a => Parsec () String (Interval a)
        singleValue = I.singleton <$> signed (return ()) decimal

        range :: Integral a => Parsec () String (Interval a)
        range = toInterval <$> try (decimal <* char '-') <*> decimal

        toInterval :: Integral a => a -> a -> Interval a
        toInterval = I.interval `on` (,True) . Finite

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
    queryModel n = Sql.validateEntity n

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
    queryPlatform (Right n) = Sql.validateEntity n
    queryPlatform (Left name) = Sql.validateUniqEntity $ UniqPlatform name

parseMispredictionStrategy :: Parsec Void String MispredictionStrategy
parseMispredictionStrategy = asum
    [ None <$ string' "none"
    , FirstInSet <$ string' "firstinset"
    , Ranked <$> columnParser <* char '-' <*> rankParser
    ]
  where
    columnParser :: Parsec Void String Column
    columnParser = asum
        [ MinTime <$ string' "min"
        , AvgTime <$ string' "avg"
        , MaxTime <$ string' "max"
        ]

    rankParser :: Parsec Void String Ranking
    rankParser = asum
        [ Min <$ string' "min"
        , Avg <$ string' "avg"
        , Total <$ string' "total"
        ]

rawPredictorConfigParser
    :: Parser (
        Maybe (Either Int Text)
        -> Maybe MispredictionStrategy
        -> SqlM PredictorConfig
    )
rawPredictorConfigParser = do
    predictorTxt <- predictorOpt
    pure $ \mDefaultImpl mStrategy -> do
        let configParser = mkConfigParser mDefaultImpl mStrategy

        case runParser configParser "" predictorTxt of
            Right v -> mkPredictorConfig v
            Left e -> logThrowM . GenericInvariantViolation $
                "Failed to parse predictor config:\n" <>
                T.pack (errorBundlePretty e)
  where
    predictorOpt :: Parser String
    predictorOpt = strOption $ mconcat
        [ metavar "ID", short 'm', long "predictor"
        , help "Predictor to use."
        ]

    mkConfigParser
        :: Maybe (Either Int Text)
        -> Maybe MispredictionStrategy
        -> Parsec Void String (Int64, Either Int Text, MispredictionStrategy)
    mkConfigParser mDefImpl mStrategy = do
        modelId <- decimal
        implId <- implIdParser

        strategy <- asum
            [ colon *> parseMispredictionStrategy
            , fromMaybe None mStrategy <$ eof
            ]

        return (modelId, implId, strategy)
      where
        colon :: Parsec Void String ()
        colon = void $ char ':'

        implIdParser :: Parsec Void String (Either Int Text)
        implIdParser = asum
            [ try $ colon *> (Left <$> numericId)
            , try $ colon *> (Right <$> textId)
            , maybe empty pure mDefImpl <* (colon <|> eof)
            ]
          where
            ending :: Parsec Void String ()
            ending = lookAhead . try $ colon <|> eof

            numericId :: Parsec Void String Int
            numericId = decimal <* ending

            textId :: Parsec Void String Text
            textId = T.pack <$> manyTill (alphaNumChar <|> char '-') ending

rawPredictorConfigSetParser
    :: Parser (
        Maybe (Either Int Text)
        -> Maybe MispredictionStrategy
        -> SqlM [PredictorConfig]
    )
rawPredictorConfigSetParser = do
    modelIntervals <- intervalFlag "predictor-set" "predictor"

    pure $ \mDefImpl mStrategy -> do
        defImpl <- case mDefImpl of
            Just v -> return v
            Nothing -> logThrowM . GenericInvariantViolation $
                "--predictor-set requires --default-impl to be specified!"

        defStrategy <- case mStrategy of
            Just v -> return v
            Nothing -> logThrowM . GenericInvariantViolation $
                "--predictor-set requires --default-strategy to be specified!"

        modelIds <- Sql.selectKeysList ([] :: [Sql.Filter PredictionModel]) []

        mapM mkPredictorConfig
            [ (modelId, defImpl, defStrategy)
            | modelId <- map fromSqlKey modelIds
            , modelId `IS.member` modelIntervals
            ]

predictorConfigParser :: Parser (SqlM PredictorConfig)
predictorConfigParser =
  rawPredictorConfigParser <*> pure Nothing <*> pure Nothing

predictorConfigsParser :: Parser (SqlM [PredictorConfig])
predictorConfigsParser = do
    defImpl <- optional implParser
    defStrategy <- optional mispredictionStrategyParser
    predictorConfigMakers <- some multiConfigParser

    pure $ concat <$> sequence
        [ makePredictorConfigs defImpl defStrategy
        | makePredictorConfigs <- predictorConfigMakers
        ]
  where
    multiConfigParser = asum
        [ (\f x y -> pure <$> f x y) <$> rawPredictorConfigParser
        , rawPredictorConfigSetParser
        ]

    implParser :: Parser (Either Int Text)
    implParser = option (Left <$> auto <|> Right <$> str) $ mconcat
        [ metavar "IMPLEMENTATION", short 'i', long "default-impl"
        , help "Default implementation in case of no valid prediction. \
            \Numeric or textual."
        ]

    mispredictionStrategyParser :: Parser MispredictionStrategy
    mispredictionStrategyParser = option mispredictionStrategyReader $
        mconcat
            [ metavar "STRATEGY", long "default-strategy"
            , help "Default strategy for handling mispredictions."
            ]
      where
        mispredictionStrategyReader :: ReadM MispredictionStrategy
        mispredictionStrategyReader =
            maybeReader $ parseMaybe parseMispredictionStrategy

renameParser :: Parser (IntMap Text)
renameParser = IM.unions <$> many renameOpt
  where
    renameOpt :: Parser (IntMap Text)
    renameOpt = option (eitherReader renameReader) $ mconcat
        [ metavar "ID:NAME", long "rename"
        , help "Specify manual name override for an implementation id" ]

    renameReader :: String -> Either String (IntMap Text)
    renameReader s = case runParser idNameParser "" s of
        Right (k, v) -> Right $ IM.singleton k v
        Left e -> Left $ errorBundlePretty e

    idNameParser :: Parsec Void String (Int, Text)
    idNameParser = do
        d <- decimal
        void $ char ':'
        name <- takeRest

        when (null name) $
            fail "Expected name with non-zero length!"

        return (d, T.pack name)

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
    queryRunConfig n = Sql.validateEntity n

requiredUtcTimeParser :: Parser (SqlM UTCTime)
requiredUtcTimeParser =
    maybe (liftIO getCurrentTime) return <$> optional utcTimeParser

setParser
    :: forall v . Ord v => Parser (SqlM v) -> Parser (SqlM (Maybe (Set v)))
setParser p = sequence <$> optional rawSetParser
  where
    rawSetParser :: Parser (SqlM (Set v))
    rawSetParser = fmap S.fromList . sequence <$> some p

utcTimeParser :: Parser UTCTime
utcTimeParser = option utcReader . mconcat $
    [ metavar "TIME", short 't', long "time"
    , help "Timestamp controlling query output." ]
  where
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
        [ metavar "ID", long "variant-config"
        , help "Numeric id of variant to use"
        ]

    variantConfigReader :: String -> Maybe (Maybe Int64)
    variantConfigReader s
        | "default" `isPrefixOf` s = Just Nothing
        | otherwise = Just <$> readMaybe s

    queryVariantConfig
        :: Maybe Int64 -> Key Algorithm -> SqlM (Entity VariantConfig)
    queryVariantConfig key algoId = case key of
        Just n -> Sql.validateEntity n
        Nothing -> Sql.selectSingle
                [ VariantConfigAlgorithmId ==. algoId
                , VariantConfigIsDefault ==. Active
                ]

variantIdParser :: Parser (Key Algorithm -> SqlM (Key Variant))
variantIdParser = fmap (fmap entityKey) <$> variantParser

variantParser :: Parser (Key Algorithm -> SqlM (Entity Variant))
variantParser = queryVariant <$> variantOpt
  where
    variantOpt :: Parser Int64
    variantOpt = option auto $ mconcat
        [ metavar "ID", long "variant", help "Numeric id of variant to use" ]

    queryVariant :: Int64 -> Key Algorithm -> SqlM (Entity Variant)
    queryVariant n algoId = Sql.selectSingle
        [ VariantId ==. toSqlKey n, VariantAlgorithmId ==. algoId ]

variantInfoConfigParser :: Parser (SqlM VariantInfoConfig)
variantInfoConfigParser = do
    getAlgoId <- algorithmIdParser
    getCommit <- commitIdParser
    getPlatformId <- platformIdParser
    getDatasets <- setParser datasetIdParser
    filterFlag <- filterIncomplete
    allowNewer <- allowNewerParser
    getUtcTime <- requiredUtcTimeParser

    pure $ do
        algoId <- getAlgoId
        VariantInfoConfig algoId
            <$> getPlatformId <*> getCommit algoId <*> pure Nothing
            <*> getDatasets <*> getUtcTime <*> pure allowNewer
            <*> pure filterFlag

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
