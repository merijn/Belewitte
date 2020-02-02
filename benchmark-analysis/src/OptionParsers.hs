{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module OptionParsers
    ( reflow
    , algorithmIdParser
    , algorithmParser
    , commitIdParser
    , datasetIdParser
    , datasetParser
    , platformIdParser
    , platformParser
    , runconfigIdParser
    , runconfigParser
    , variantIdParser
    , variantParser
    , intervalReader
    , readCI
    , mapOption
    , optionEnumerationHelp
    , optionParserFromValues
    , module Options.Applicative
    ) where

import Data.Char (toLower)
import Data.Function (on)
import Data.Int (Int64)
import Data.Interval (Extended(Finite), Interval)
import qualified Data.Interval as I
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Options.Applicative hiding (Completer)
import Options.Applicative.Help (Doc, (</>))
import qualified Options.Applicative.Help as Help
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

import Core
import FieldQuery (getDistinctFieldLikeQuery)
import Query (runSqlQuerySingle)
import Schema
import Sql (Key, Entity(..))
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
    queryAlgorithm (Right n) = Sql.validateEntity "Algorithm" n
    queryAlgorithm (Left name) = Sql.validateUniqEntity "Algorithm" $
        UniqAlgorithm name

commitIdParser :: Parser (SqlM CommitId)
commitIdParser = checkUniqueCommitId <$> commitIdOpt
  where
    commitIdOpt :: Parser Text
    commitIdOpt = option str $ mconcat
        [ metavar "COMMIT", short 'c', long "commit"
        , help "Algorithm version to use" ]

    checkUniqueCommitId :: Text -> SqlM CommitId
    checkUniqueCommitId txt = do
        query <- getDistinctFieldLikeQuery RunConfigAlgorithmVersion txt
        runSqlQuerySingle query

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
