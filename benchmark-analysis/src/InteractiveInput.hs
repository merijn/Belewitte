{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module InteractiveInput
    ( Completer(..)
    , Input
    , runInput
    , liftSql
    , withCompletion
    , withProcessCompletion
    , getInputWith
    , getInputWithSqlCompletion
    , getOptionalInput
    , getReadInput
    , getInput
    ) where

import qualified Control.Monad.Catch as Except
import Control.Monad.Reader (ask, local)
import Control.Monad.Trans (lift)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Database.Persist.Sqlite (Entity(..), EntityField, Unique)
import qualified Database.Persist.Sqlite as Sql
import Lens.Micro.Extras (view)
import System.Console.Haskeline hiding (Handler)
import Text.Read (readMaybe)

import Core
import Utils.Process (CreateProcess, UnexpectedTermination(..))
import qualified Utils.Process as Process
import qualified RuntimeData

data Completer m
    = SimpleCompletion (String -> m [Completion])
    | FileCompletion

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

getInputWith
    :: (MonadException m, MonadLogger m, MonadThrow m)
    => (Text -> m (Maybe a)) -> Text -> Text -> InputT m a
getInputWith convert errText prompt = go
  where
    go = getInputLine (T.unpack prompt ++ ": ") >>= \case
            Nothing -> lift $ logThrowM StdinDisappeared
            Just s -> lift (convert . T.stripEnd . T.pack $ s) >>= \case
                Just r -> return r
                Nothing -> outputStrLn (T.unpack errText) >> go

getInputWithSqlCompletion
    :: SqlRecord record
    => EntityField record Text
    -> (Text -> Unique record)
    -> Text
    -> Input SqlM (Entity record)
getInputWithSqlCompletion field uniq prompt =
  withCompletion (SimpleCompletion fromQuery) $
    getInputWith (lift . Sql.getBy . uniq) err prompt
  where
    err = "Name not found in database!"
    getFieldValue = view (Sql.fieldLens field)
    toCompletions = map $ simpleCompletion . T.unpack . getFieldValue
    fromQuery s =
      toCompletions <$> Sql.selectList [field `likeFilter` T.pack s] []

getOptionalInput
    :: (MonadException m, MonadLogger m, MonadThrow m)
    => Text -> InputT m (Maybe Text)
getOptionalInput = getInputWith checkEmpty "" -- Never fails
  where
    checkEmpty :: MonadException m => Text -> m (Maybe (Maybe Text))
    checkEmpty txt
        | T.null txt = return $ Just Nothing
        | otherwise = return . Just . Just $ txt

getReadInput
    :: forall a m .
    ( MonadException m
    , MonadLogger m
    , MonadThrow m
    , Bounded a
    , Enum a
    , Read a
    , Show a
    ) => (a -> Bool) -> Text -> Input m a
getReadInput f prompt = withCompletion (SimpleCompletion completions) $
    getInputWith (return . readMaybe . T.unpack) "Parse error!" prompt
  where
    allValues :: [a]
    allValues = filter f [minBound .. maxBound]

    completions :: Monad m => String -> m [Completion]
    completions s = return $ toCompletions s allValues

    toCompletions :: Show x => String -> [x] -> [Completion]
    toCompletions s = map simpleCompletion . filter (isPrefix s) . map show
      where
        isPrefix = isPrefixOf `on` map toLower

getInput
    :: (MonadException m, MonadLogger m, MonadThrow m) => Text -> InputT m Text
getInput = getInputWith checkEmpty "Empty input not allowed!"
  where
    checkEmpty :: MonadException m => Text -> m (Maybe Text)
    checkEmpty txt
        | T.null txt = return Nothing
        | otherwise = return $ Just txt
