{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands
    ( Command
    , CommandInfo(..)
    , pattern CommandGroup
    , pattern CommandGroupWithFlags
    , pattern CommandWithSubGroup
    , pattern HiddenGroup
    , pattern HiddenCommand
    , pattern SingleCommand
    , buildCommand
    ) where

import Options.Applicative hiding (Completer)
import Options.Applicative.Help (Doc)
import qualified Options.Applicative.Help as Help

import Core (mIf)

reflowWithMaxWidth :: Int -> String -> Doc
reflowWithMaxWidth maxWidth = foldMap wordToDoc . words
  where
    wordToDoc :: String -> Doc
    wordToDoc s = Help.column maybeLine <> Help.text s <> Help.softline
      where
        maybeLine c = mIf (c + length s >= maxWidth) Help.hardline

data CommandInfo = CommandInfo
    { commandName :: String
    , commandHeaderDesc :: String
    , commandDesc :: String
    }

pattern HiddenCommand :: CommandInfo -> Parser a -> Command a
pattern HiddenCommand info parser = Hidden (Command info (Single parser))

pattern SingleCommand :: CommandInfo -> Parser a -> Command a
pattern SingleCommand info parser = Command info (Single parser)

pattern CommandWithSubGroup
    :: CommandInfo -> Parser a -> [Command a] -> Command a
pattern CommandWithSubGroup info parser cmds =
    Command info (WithSubGroup parser cmds)

pattern CommandGroup :: CommandInfo -> [Command a] -> Command a
pattern CommandGroup info cmds = Command info (Group cmds)

pattern CommandGroupWithFlags
    :: CommandInfo -> Parser b -> [Command (b -> a)] -> Command a
pattern CommandGroupWithFlags info flags cmds =
  Command info (WithFlags flags cmds)

pattern HiddenGroup :: CommandInfo -> [Command a] -> Command a
pattern HiddenGroup info cmds = Hidden (CommandGroup info cmds)

data CommandType a where
    Single :: Parser a -> CommandType a
    WithSubGroup :: Parser a -> [Command a] -> CommandType a
    WithFlags :: Parser b -> [Command (b -> a)] -> CommandType a
    Group :: [Command a] -> CommandType a

instance Functor CommandType where
    fmap f cmd = case cmd of
        Single parser -> Single (f <$> parser)
        WithSubGroup parser cmds ->
            WithSubGroup (f <$> parser) $ map (fmap f) cmds
        WithFlags parser cmds -> WithFlags parser $ map (fmap (fmap f)) cmds
        Group cmds -> Group $ map (fmap f) cmds

data Command a = Command CommandInfo (CommandType a) | Hidden (Command a)
    deriving (Functor)

buildCommand :: Command a -> (Parser a, InfoMod b)
buildCommand cmd = (parser, infoMod)
  where
    (_, parser, infoMod) = unfoldCommand "" cmd

unfoldCommand :: String -> Command a -> (String, Parser a, InfoMod b)
unfoldCommand prefix (Hidden cmd) = unfoldCommand prefix cmd
unfoldCommand prefix (Command CommandInfo{..} cmdType) =
    (commandName, parser, infoMod)
  where
    groupPrefix :: String
    groupPrefix = prefix ++ commandName ++ " "

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
        WithSubGroup p cmds -> p <|> groupToParser cmds groupPrefix
        WithFlags p cmds -> groupToParser cmds groupPrefix <*> p
        Group cmds -> groupToParser cmds groupPrefix

groupToParser :: forall a . [Command a] -> String -> Parser a
groupToParser cmds prefix = groupParser
  where
    groupParser :: Parser a
    groupParser = hsubparser cmdGroup <|> hsubparser (hiddenCmdGroup <> internal)

    cmdGroup, hiddenCmdGroup :: Mod CommandFields a
    (cmdGroup, hiddenCmdGroup) = foldMap unfoldSubCommand cmds

    unfoldSubCommand :: Command a -> (Mod CommandFields a, Mod CommandFields a)
    unfoldSubCommand cmd = select . wrapCommand . unfoldCommand prefix $ cmd
      where
        select cmdFields = case cmd of
            Hidden _ -> (mempty, cmdFields)
            _ -> (cmdFields, mempty)

    wrapCommand :: (String, Parser a, InfoMod a) -> Mod CommandFields a
    wrapCommand (cmd, parser, infoMod) = command cmd $ info parser infoMod
