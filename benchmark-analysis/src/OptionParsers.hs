{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module OptionParsers
    ( platformIdParser
    , platformParser
    , algorithmIdParser
    , algorithmParser
    , intervalReader
    , optionParserFromValues
    , runSqlM
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
import qualified Data.Text as T
import Options.Applicative hiding (Completer)
import System.Environment (getProgName)
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

import Core
import Schema
import Sql (Key, Entity(..))
import qualified Sql

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
    queryPlatform (Right n) = do
        Just entity <- logIfFail "No Platform with id" (showText n) $
                Sql.getEntity $ toSqlKey n
        return entity
    queryPlatform (Left name) = do
        Just entity <- logIfFail "No Platform with name" name $
            Sql.getBy $ UniqPlatform name
        return entity

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
    queryAlgorithm (Right n) = do
        Just entity <- logIfFail "No algorithm with id" (showText n) $
                Sql.getEntity $ toSqlKey n
        return entity
    queryAlgorithm (Left name) = do
        Just entity <- logIfFail "No algorithm with name" name $
                Sql.getBy $ UniqAlgorithm name
        return entity

intervalReader :: ReadM (IntervalSet Int64)
intervalReader = maybeReader . parseMaybe $
    IS.fromList <$> sepBy1 interval (char ',')
  where
    interval :: Parsec () String (Interval Int64)
    interval = range <|> singleValue

    singleValue = I.singleton <$> decimal
    range = toInterval <$> try (decimal <* char '-') <*> decimal

    toInterval = I.interval `on` (,True) . Finite

optionParserFromValues
    :: Map String a -> Mod OptionFields a -> Parser a
optionParserFromValues vals = option . maybeReader $ lookupCI
  where
    lookupCI key = M.lookup (map toLower key) valMap
    valMap = M.mapKeys (map toLower) vals

runSqlM :: (String -> (InfoMod (Options a), Parser a)) -> (a -> SqlM b) -> IO b
runSqlM configFromName work = do
    (helpInfo, parser) <- configFromName <$> getProgName
    config <- execParser $ info (mkOptions parser <**> helper) helpInfo
    runSqlMWithOptions config work
  where
    mkOptions p =
      Options <$> databaseOption <*> vacuumOption <*> verbosityOption
              <*> queryOption <*> migrateOption <*> p

    vacuumOption = flag False True $ mconcat
        [ long "vacuum-db"
        , help "Vacuum SQLite database to compact it and speed up." ]

    migrateOption = flag False True $ mconcat
        [ help "Automatically migrate old schema to current version. Caution!"
        , long "migrate" ]

databaseOption :: Parser Text
databaseOption = strOption . mconcat $
    [ metavar "DATABASE", short 'd', long "database"
    , value "benchmarks.db", help "Path of SQLite database to use."
    , showDefaultWith T.unpack
    ]

queryOption :: Parser QueryMode
queryOption = explainFlag <|> ExplainLog <$> explainOpt <|> pure Normal
  where
    explainFlag = flag' Explain $ mconcat
        [ long "explain", help "Log query plans to stdout." ]

    explainOpt = strOption . mconcat $
        [ metavar "FILE", long "explain-log"
        , help "Log query plans to log file." ]

verbosityOption :: Parser Int
verbosityOption = quiet <|> verb
  where
    verb = option auto . mconcat $
        [ short 'v', long "verbose", help "Enable more verbose logging."
        , value 1, metavar "N", showDefault ]

    quiet = flag' 0 . mconcat $
        [ short 'q', long "quiet", help "Disable all logging." ]
