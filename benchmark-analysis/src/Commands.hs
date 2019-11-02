{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands
    ( Command
    , CommandInfo(..)
    , pattern CommandGroup
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
