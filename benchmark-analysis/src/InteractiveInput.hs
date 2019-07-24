{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module InteractiveInput
    ( Completer(..)
    , Input
    , runInput
    , liftSql
    , getInteractive
    , getManyInteractive
    , processCompleterText
    , filepathInput
    , sqlInput
    , optionalInput
    , readInput
    , textInput
    ) where

import qualified Control.Monad.Catch as Except
import Control.Monad.Reader (ask, local)
import Control.Monad.Trans (lift)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Database.Persist.Sqlite (Entity(..), EntityField, Unique)
import qualified Database.Persist.Sqlite as Sql
import Lens.Micro.Extras (view)
import System.Console.Haskeline hiding (Handler)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

import Core
import Utils.Process (CreateProcess, UnexpectedTermination(..))
import qualified Utils.Process as Process
import qualified RuntimeData

data Completer m
    = SimpleCompletion (String -> m [Completion])
    | FileCompletion

data InputQuery m a = InputQuery
    { inputConvert :: Text -> m (Maybe a)
    , inputCompleter :: forall x . Input m x -> Input m x
    , inputError :: Text
    }

type Input m = InputT (ReaderT (Completer m) m)

dynCompleter :: CompletionFunc (ReaderT (Completer SqlM) SqlM)
dynCompleter completeArgs = do
    ask >>= \case
        SimpleCompletion f -> lift $ completeWord Nothing " \t" f completeArgs
        FileCompletion -> completeFilename completeArgs

runInput :: Input SqlM a -> SqlM a
runInput = (`runReaderT` emptyCompletion) . runInputT settings
  where
    emptyCompletion = SimpleCompletion $ const (return [])
    settings = setComplete dynCompleter defaultSettings

liftSql :: SqlM a -> Input SqlM a
liftSql = lift . lift

withCompletion :: Monad m => Completer m -> Input m a -> Input m a
withCompletion completion = mapInputT $ local (const completion)

withProcessCompletion
    :: (MonadLogger m, MonadMask m, MonadUnliftIO m)
    => [String] -> Input m a -> Input m a
withProcessCompletion args act = do
    completionBracket <- mkCompletionBracket
        <$> RuntimeData.getKernelExecutableMaybe
        <*> RuntimeData.getKernelLibPathMaybe

    completionBracket act
  where
    mkCompletionBracket (Just exe) (Just libDir) =
        withCompletion (SimpleCompletion $ processCompleter process)
      where
        process :: CreateProcess
        process = Process.proc exe $ ["-L",libDir] ++ args

    mkCompletionBracket _ _ = id

    processCompleter process s = do
        txt <- warnOnError $ Process.readStdout process
        let relevant = filter (T.isPrefixOf (T.pack s)) $ T.lines txt
        return $ map (simpleCompletion . T.unpack) relevant
      where
        warnOnError
            :: (MonadCatch m, MonadIO m, MonadLogger m) => m Text -> m Text
        warnOnError runProcess = runProcess `Except.catches`
            [ Except.Handler handleUnexpectedTermination
            , Except.Handler handleIOException
            ]

        handleUnexpectedTermination
            :: MonadLogger m => UnexpectedTermination -> m Text
        handleUnexpectedTermination (UnexpectedTermination _) =
            "" <$ logWarnN "External tab-completion process failed to run"

        handleIOException :: MonadLogger m => IOException -> m Text
        handleIOException _ =
            "" <$ logWarnN "External tab-completion process failed to run"

getInteractive
    :: forall a m . (MonadException m, MonadIO m, MonadLogger m, MonadThrow m)
    => InputQuery m a -> Text -> Input m a
getInteractive InputQuery{..} prompt = inputCompleter go
  where
    go = getInputLine (T.unpack prompt ++ ": ") >>= \case
        Nothing -> lift $ logThrowM StdinDisappeared
        Just s -> do
            converter s >>= \case
                Just r -> return r
                Nothing -> outputStrLn (T.unpack inputError) >> go

    converter s = lift . lift $ inputConvert . T.stripEnd . T.pack $ s

getManyInteractive
    :: forall a m . (MonadException m, MonadIO m, MonadLogger m, MonadThrow m)
    => InputQuery m a -> Text -> Input m [a]
getManyInteractive InputQuery{..} prompt = inputCompleter $ do
    outputStrLn (T.unpack prompt ++ ":\n")
    go id
  where
    go results = getInputLine "> " >>= \case
        Nothing -> lift $ logThrowM StdinDisappeared
        Just "" -> return $ results []
        Just s -> converter s >>= \case
                Just r -> go (results . (r:))
                Nothing -> do
                    outputStrLn (T.unpack inputError <> "Empty line to end.")
                    go results

    converter s = lift . lift $ inputConvert . T.stripEnd . T.pack $ s


processCompleterText
    :: (MonadLogger m, MonadMask m, MonadUnliftIO m)
    => [String] -> InputQuery m Text
processCompleterText args = InputQuery
    { inputConvert = checkEmpty
    , inputCompleter = withProcessCompletion args
    , inputError = "Name not found in database!"
    }
  where
    checkEmpty :: Applicative m => Text -> m (Maybe Text)
    checkEmpty txt
        | T.null txt = pure Nothing
        | otherwise = pure $ Just txt

filepathInput
    :: MonadIO m => InputQuery m FilePath
filepathInput = InputQuery
    { inputConvert = checkExists
    , inputCompleter = withCompletion FileCompletion
    , inputError = "Non-existent file!"
    }
  where
    checkExists :: MonadIO m => Text -> m (Maybe FilePath)
    checkExists txt = bool Nothing (Just path) <$> liftIO (doesFileExist path)
      where
        path = T.unpack txt

sqlInput
    :: SqlRecord record
    => EntityField record Text
    -> (Text -> Unique record)
    -> InputQuery SqlM (Entity record)
sqlInput field uniq = InputQuery
    { inputConvert = Sql.getBy . uniq
    , inputCompleter = withCompletion (SimpleCompletion fromQuery)
    , inputError = "Name not found in database!"
    }
  where
    getFieldValue = view (Sql.fieldLens field)
    toCompletions = map $ simpleCompletion . T.unpack . getFieldValue
    fromQuery s =
      toCompletions <$> Sql.selectList [field `likeFilter` T.pack s] []

optionalInput
    :: (Applicative m)
    => InputQuery m (Maybe Text)
optionalInput = InputQuery
    { inputConvert = checkEmpty
    , inputCompleter = id
    , inputError = ""
    }
  where
    checkEmpty :: Applicative m => Text -> m (Maybe (Maybe Text))
    checkEmpty txt
        | T.null txt = pure $ Just Nothing
        | otherwise = pure . Just . Just $ txt

readInput
    :: forall a m . (Bounded a, Enum a, Read a, Show a, Monad m)
    => (a -> Bool) -> InputQuery m a
readInput f = InputQuery
    { inputConvert = return . readMaybe . T.unpack
    , inputCompleter = withCompletion (SimpleCompletion completions)
    , inputError = "Parse error!"
    }
  where
    allValues :: [a]
    allValues = filter f [minBound .. maxBound]

    completions :: Monad m => String -> m [Completion]
    completions s = return $ toCompletions s allValues

    toCompletions :: Show x => String -> [x] -> [Completion]
    toCompletions s = map simpleCompletion . filter (isPrefix s) . map show
      where
        isPrefix = isPrefixOf `on` map toLower

textInput :: Applicative m => InputQuery m Text
textInput = InputQuery
    { inputConvert = checkEmpty
    , inputCompleter = id
    , inputError = "Empty input not allowed!"
    }
  where
    checkEmpty :: Applicative m => Text -> m (Maybe Text)
    checkEmpty txt
        | T.null txt = pure Nothing
        | otherwise = pure $ Just txt