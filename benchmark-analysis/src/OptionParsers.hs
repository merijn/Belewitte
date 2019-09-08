{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module OptionParsers
    ( CommandInfo(..)
    , Command
    , pattern HiddenCommand
    , pattern SingleCommand
    , pattern CommandGroup
    , reflow
    , reflowWithMaxWidth
    , platformIdParser
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
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative hiding (Completer)
import Options.Applicative.Help (Doc)
import qualified Options.Applicative.Help as Help
import System.Environment (getProgName)
import qualified System.IO as System
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

import Core
import Schema
import Sql (Key, Entity(..))
import qualified Sql

newtype SubCommands = SubCommands [(String, Doc)]
    deriving (Semigroup, Monoid, Show)

data CommandInfo = CommandInfo
    { commandName :: String
    , commandHeaderDesc :: String
    , commandDesc :: String
    }

pattern HiddenCommand :: CommandInfo -> Parser a -> Command a
pattern HiddenCommand info parser = Hidden (Command info (Single parser))

pattern SingleCommand :: CommandInfo -> Parser a -> Command a
pattern SingleCommand info parser = Command info (Single parser)

pattern CommandGroup :: CommandInfo -> [Command a] -> Command a
pattern CommandGroup info cmds = Command info (Group cmds)

data CommandType a = Single (Parser a) | Group [Command a]
    deriving (Functor)

data Command a = Command CommandInfo (CommandType a) | Hidden (Command a)
    deriving (Functor)

buildCommand :: Command a -> (Parser a, InfoMod b)
buildCommand cmd = (parser, infoMod)
  where
    (_, parser, infoMod) = unfoldCommand "" cmd

unfoldCommand :: String -> Command a -> (String, Parser a, InfoMod b)
unfoldCommand prefix (Hidden cmd) = unfoldCommand prefix cmd
unfoldCommand prefix (Command cmdInfo@CommandInfo{..} cmdType) =
    (commandName, parser, infoMod)
  where
    justUnless :: Bool -> v -> Maybe v
    justUnless b v
        | not b = Just v
        | otherwise = Nothing

    infoMod :: InfoMod a
    infoMod = mconcat
        [ fullDesc
        , header $ prefix ++ commandName ++ " - " ++ commandHeaderDesc
        , progDescDoc . justUnless (null commandDesc) $ mconcat
            [ mIf (null prefix) Help.linebreak
            , reflowWithMaxWidth 80 commandDesc
            ]
        , footerDoc . justUnless (null subcmds) $ subcommandBlocks subcmds
        ]

    subcommandBlocks :: [(String, Doc)] -> Doc
    subcommandBlocks = mconcat . intersperse separator . map toSubCommandBlock
        where
          separator = Help.hardline <> Help.hardline

    toSubCommandBlock :: (String, Doc) -> Doc
    toSubCommandBlock (title, doc) = mconcat
        [ "Subcommands for \"" <> Help.text title <> "\":"
        , Help.hardline
        , doc
        ]

    (parser, SubCommands subcmds) = case cmdType of
        Single p -> (p, mempty)
        Group cmds -> groupToParser cmdInfo cmds prefix

groupToParser
    :: forall a
     . CommandInfo
    -> [Command a]
    -> String
    -> (Parser a, SubCommands)
groupToParser CommandInfo{..} cmds prefix = (groupParser, subcommands)
  where
    groupPrefix :: String
    groupPrefix = prefix ++ commandName ++ " "

    groupParser :: Parser a
    groupParser = hsubparser cmdGroup <|> hsubparser (hiddenCmdGroup <> internal)

    cmdGroup, hiddenCmdGroup :: Mod CommandFields a
    subcommands :: SubCommands
    (cmdGroup, hiddenCmdGroup, subcommands) = foldMap unfoldSubCommand cmds

    unfoldSubCommand
        :: Command a -> (Mod CommandFields a, Mod CommandFields a, SubCommands)
    unfoldSubCommand cmd = select . wrapCommand . unfoldCommand groupPrefix $ cmd
      where
        select (cmdFields, subcmds) = case cmd of
            Hidden _ -> (mempty, cmdFields, subcmds)
            _ -> (cmdFields, mempty, subcmds)

    wrapCommand
        :: (String, Parser a, InfoMod a) -> (Mod CommandFields a, SubCommands)
    wrapCommand (cmd, parser, infoMod) = (cmdFields, subcmds)
      where
        cmdFields = command cmd $ info parser infoMod
        docs = mapMaybe (Help.unChunk . snd) $ Help.cmdDesc parser
        subcmds = case docs of
            [] -> mempty
            _ -> SubCommands [(cmd, mconcat docs)]

reflow :: String -> Doc
reflow = Help.fillSep . map Help.text . words

reflowWithMaxWidth :: Int -> String -> Doc
reflowWithMaxWidth maxWidth = foldMap wordToDoc . words
  where
    wordToDoc :: String -> Doc
    wordToDoc s = Help.column maybeLine <> Help.text s <> Help.softline
      where
        maybeLine c = mIf (c + length s >= maxWidth) Help.hardline

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

runSqlM :: (String -> Command a) -> (a -> SqlM b) -> IO b
runSqlM commandFromName work = do
    System.hFlush System.stdout >> System.hFlush System.stderr

    (parser, helpInfo) <- buildCommand . commandFromName <$> getProgName
    cols <- fromMaybe 80 <$> terminalWidth

    let topParser = mkOptions parser <**> helper
        parseInfo = info topParser helpInfo
        parsePrefs = prefs $ columns cols

    config <- customExecParser parsePrefs parseInfo
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
