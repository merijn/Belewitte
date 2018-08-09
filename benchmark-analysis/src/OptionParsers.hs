{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module OptionParsers
    ( intervalReader
    , optionParserFromValues
    , runSqlM
    , module Options.Applicative
    ) where

import Control.Monad.Logger (LogLevel(..), LogSource)
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
import Options.Applicative
import System.Environment (getProgName)
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

import Core (Options(..), SqlM, runSqlMWithOptions)

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
    let options = Options <$> databaseOption <*> verbosityOption <*> parser
    config <- execParser $ info (options <**> helper) helpInfo
    runSqlMWithOptions config work

databaseOption :: Parser Text
databaseOption = strOption . mconcat $
    [ metavar "DATABASE", short 'd', long "database"
    , value "benchmarks.db", help "Path of SQLite database to use."
    , showDefaultWith T.unpack
    ]

verbosityOption :: Parser (LogSource -> LogLevel -> Bool)
verbosityOption = logFilter . (levels !!) <$> verb
  where
    logFilter verbosity = \_ lvl -> lvl >= verbosity

    levels = LevelError : LevelWarn : LevelInfo : repeat LevelDebug

    verb = option auto . mconcat $
        [ short 'v', long "verbose", help "Enable more verbose logging."
        , value 0, metavar "N", showDefault ]
