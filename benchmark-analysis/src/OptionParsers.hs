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
    , readCI
    , mapOption
    , optionEnumerationHelp
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative hiding (Completer)
import Options.Applicative.Help (Doc, (</>))
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
        ]

    parser = case cmdType of
        Single p -> p
        Group cmds -> groupToParser cmdInfo cmds prefix

groupToParser
    :: forall a
     . CommandInfo
    -> [Command a]
    -> String
    -> Parser a
groupToParser CommandInfo{..} cmds prefix = groupParser
  where
    groupPrefix :: String
    groupPrefix = prefix ++ commandName ++ " "

    groupParser :: Parser a
    groupParser = hsubparser cmdGroup <|> hsubparser (hiddenCmdGroup <> internal)

    cmdGroup, hiddenCmdGroup :: Mod CommandFields a
    (cmdGroup, hiddenCmdGroup) = foldMap unfoldSubCommand cmds

    unfoldSubCommand :: Command a -> (Mod CommandFields a, Mod CommandFields a)
    unfoldSubCommand cmd = select . wrapCommand . unfoldCommand groupPrefix $ cmd
      where
        select cmdFields = case cmd of
            Hidden _ -> (mempty, cmdFields)
            _ -> (cmdFields, mempty)

    wrapCommand :: (String, Parser a, InfoMod a) -> Mod CommandFields a
    wrapCommand (cmd, parser, infoMod) = command cmd $ info parser infoMod

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

runSqlM :: (String -> Command a) -> (a -> SqlM b) -> IO b
runSqlM commandFromName work = do
    System.hFlush System.stdout >> System.hFlush System.stderr

    (parser, helpInfo) <- buildCommand . commandFromName <$> getProgName
    cols <- fromMaybe 80 <$> stderrTerminalWidth

    let topParser = mkOptions parser <**> helper
        parseInfo = info topParser helpInfo
        parsePrefs = prefs $ columns cols

    config <- customExecParser parsePrefs parseInfo
    runSqlMWithOptions config work
  where
    mkOptions p =
      Options <$> databaseOption <*> vacuumOption <*> verbosityOption
              <*> queryOption <*> migrateOption <*> pagerOption <*> p

    pagerOption :: Parser Pager
    pagerOption = optionParserFromValues pagerValues "PAGER" helpTxt $ mconcat
        [ long "pager", value Auto, showDefaultWith (map toLower . show) ]
      where
        helpTxt = "Controls whether output is paged."

        pagerValues :: Map String Pager
        pagerValues = M.fromList
            [("never", Never), ("auto", Auto), ("always", Always)]

    vacuumOption :: Parser Bool
    vacuumOption = flag False True $ mconcat
        [ long "vacuum-db"
        , help "Vacuum SQLite database to compact it and speed up." ]

    migrateOption :: Parser Bool
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
