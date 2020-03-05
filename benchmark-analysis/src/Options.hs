{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module Options
    ( CommandRoot(..)
    , Command
    , CommandInfo(..)
    , Compose(Compose)
    , DebugQuery(..)
    , pattern CommandGroup
    , pattern CommandWithSubGroup
    , pattern HiddenGroup
    , pattern HiddenCommand
    , pattern SingleCommand
    , nameDebugQuery
    , runSqlM
    , module OptionParsers
    ) where

import Control.Monad.Logger (LogLevel(..))
import Data.Char (toLower)
import Data.Functor.Compose (Compose(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import System.Environment (getProgName)
import qualified System.IO as System
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, takeWhile1P)
import Text.Megaparsec.Char (char)

import Core
import Commands
import Commands.Debug (DebugQuery(..))
import qualified Commands.Debug as Debug
import OptionParsers
import Query (Query)

data CommandRoot a = CommandRoot
    { mainHeaderDesc :: String
    , mainDesc :: String
    , mainQueryDump :: FilePath -> SqlM ()
    , mainQueryMap :: Map String (Parser DebugQuery)
    , mainCommands :: [Command a]
    }

nameDebugQuery
    :: Show v
    => String
    -> Compose Parser SqlM (Query v)
    -> (String, Parser DebugQuery)
nameDebugQuery name (Compose queryParser) = (name, DebugQuery <$> queryParser)

runCommandRoot :: CommandRoot a -> (a -> SqlM ()) -> String -> IO ()
runCommandRoot CommandRoot{..} work progName = do
    System.hFlush System.stdout >> System.hFlush System.stderr

    cols <- fromMaybe 80 <$> stderrTerminalWidth

    let parsePrefs = prefs $ columns cols

    config <- customExecParser parsePrefs parseInfo
    runSqlMWithOptions config (either id work)
  where
    debugCommand = Left <$> Debug.commands mainQueryDump mainQueryMap

    mainCommand = CommandGroup CommandInfo
        { commandName = progName
        , commandHeaderDesc = mainHeaderDesc
        , commandDesc = mainDesc
        }
        $ map (fmap Right) mainCommands <> [debugCommand]

    parseInfo = info (optionParser <**> helper) helpInfo

    (parser, helpInfo) = buildCommand mainCommand

    optionParser =
      Options <$> databaseOption <*> vacuumOption <*> verbosityOption
              <*> debugOption <*> explainOption <*> migrateOption
              <*> pagerOption <*> parser

runSqlM :: CommandRoot a -> (a -> SqlM ()) -> IO ()
runSqlM root work = getProgName >>= runCommandRoot root work

databaseOption :: Parser Text
databaseOption = strOption . mconcat $
    [ metavar "DATABASE", short 'd', long "database"
    , value "benchmarks.db", help "Path of SQLite database to use."
    , showDefaultWith T.unpack
    ]

debugOption :: Parser (Maybe (Set Text))
debugOption = textSetOption . mconcat $
    [metavar "PREFIX", long "debug", help "Debug prefixes to log/report."]

explainOption :: Parser (Maybe (Set Text))
explainOption = textSetOption . mconcat $
    [ metavar "QUERY", long "explain"
    , help "Log query plans for specified names to stdout."
    ]

migrateOption :: Parser Bool
migrateOption = flag False True $ mconcat
    [ help "Automatically migrate old schema to current version. Caution!"
    , long "migrate" ]

pagerOption :: Parser Pager
pagerOption = optionParserFromValues pagerValues "PAGER" helpTxt $ mconcat
    [ long "pager", value Auto, showDefaultWith (map toLower . show) ]
  where
    helpTxt = "Controls whether output is paged."

    pagerValues :: Map String Pager
    pagerValues = M.fromList
        [("never", Never), ("auto", Auto), ("always", Always)]

textSetOption
    :: Mod OptionFields (Maybe (Set Text)) -> Parser (Maybe (Set Text))
textSetOption opts = combine <$> many (option reader opts)
  where
    reader :: ReadM (Maybe (Set Text))
    reader = allReader <|> Just <$> textSetReader

    combine :: [Maybe (Set Text)] -> Maybe (Set Text)
    combine sets = S.unions <$> sequence sets

    allReader :: ReadM (Maybe a)
    allReader = maybeReader $ \s -> case map toLower s of
        "all" -> Just Nothing
        _ -> Nothing

    textSetReader :: ReadM (Set Text)
    textSetReader = fmap S.fromList . maybeReader . parseMaybe $
        map (T.toLower . T.pack) <$> sepBy1 commaFreeString (char ',')
      where
        commaFreeString :: Parsec Void String String
        commaFreeString = takeWhile1P Nothing (/=',')

vacuumOption :: Parser Bool
vacuumOption = flag False True $ mconcat
    [ long "vacuum-db"
    , help "Vacuum SQLite database to compact it and speed up." ]

verbosityOption :: Parser LogLevel
verbosityOption = quiet <|> verb <|> pure LevelWarn
  where
    verb = flag' LevelInfo . mconcat $
        [ short 'v', long "verbose", help "Enable more verbose logging." ]

    quiet = flag' LevelError . mconcat $
        [ short 'q', long "quiet", help "Quieter logging." ]
