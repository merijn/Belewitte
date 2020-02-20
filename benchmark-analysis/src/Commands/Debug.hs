{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Debug (DebugQuery(..), commands) where

import Data.ByteString.Base64 (encodeBase64)
import Data.Char (isSpace)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Functor.Compose (Compose(..))
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Numeric (showGFloat)

import Commands
import Core
import InteractiveInput
import OptionParsers
import Query (Query(..), explainSqlQuery, runSqlQueryConduit)
import Schema (PersistValue(..))
import VariantQuery (VariantInfo, VariantInfoConfig(..), variantInfoQuery)

data DebugQuery where
    DebugQuery :: Show v => SqlM (Query v) -> DebugQuery

commands
    :: (FilePath -> SqlM ())
    -> Map String (Parser DebugQuery)
    -> Command (SqlM ())
commands dumpCommand queryMap = HiddenGroup CommandInfo
    { commandName = "debug"
    , commandHeaderDesc = "debug and testing commands"
    , commandDesc = "Various subcommands for testing and debugging issues."
    } subcommands
  where
    subcommands =
        [ SingleCommand CommandInfo
            { commandName = "dump"
            , commandHeaderDesc = "dump query results to file"
            , commandDesc = "Dump the query results to files"
            }
            $ dumpCommand <$> suffixParser
        , CommandGroup CommandInfo
            { commandName = "interactive"
            , commandHeaderDesc = "interactive queries from commandline"
            , commandDesc = "Interactively read queries from commandline"
            }
            [ SingleCommand CommandInfo
                { commandName = "explain"
                , commandHeaderDesc = "print query explanation to stdout"
                , commandDesc =
                  "Read query from commandline and print explanation to stdout"
                }
                $ pure (runInput explainQuery)
            , SingleCommand CommandInfo
                { commandName = "query"
                , commandHeaderDesc = "print query results to stdout"
                , commandDesc =
                  "Read query from commandline and print results to stdout"
                }
                $ pure (runInput testQuery)
            ]
        ] ++ buildQueryList debugQueryCommand

    completeQueryMap :: Map String (Parser DebugQuery)
    completeQueryMap =
      M.insert "variantInfoQuery" (DebugQuery <$> variantQuery) queryMap

    variantQuery :: Parser (SqlM (Query VariantInfo))
    variantQuery = getCompose . fmap variantInfoQuery $ VariantInfoConfig
        <$> Compose algorithmIdParser <*> Compose platformIdParser
        <*> Compose commitIdParser <*> optional (Compose datasetIdParser)
        <*> Compose (return <$> filterIncomplete)

    buildQueryList :: (String -> Parser DebugQuery -> a) -> [a]
    buildQueryList f = M.foldMapWithKey (\k v -> [f k v]) completeQueryMap

    suffixReader :: String -> Maybe String
    suffixReader "" = Nothing
    suffixReader s
        | any isSpace s = Nothing
        | otherwise = Just $ s

    suffixParser :: Parser String
    suffixParser = argument (maybeReader suffixReader) . mconcat $
        [ metavar "SUFFIX" ]

debugQueryCommand :: String -> Parser (DebugQuery) -> Command (SqlM ())
debugQueryCommand name flags = CommandGroupWithFlags CommandInfo
    { commandName = name
    , commandHeaderDesc = "debug commands for: " ++ name
    , commandDesc = "Debug commands for: " ++ name
    }
    flags
    [explainQueryCommand, queryCommand, timeQueryCommand]

explainQueryCommand :: Command (DebugQuery -> SqlM ())
explainQueryCommand = SingleCommand CommandInfo
    { commandName = "explain"
    , commandHeaderDesc = "query plan"
    , commandDesc = "Show the query plan"
    }
    $ pure explainDebugQuery
  where
    explainDebugQuery :: DebugQuery -> SqlM ()
    explainDebugQuery (DebugQuery getQuery) = do
        query <- getQuery
        explanation <- explainSqlQuery query
        liftIO $ T.putStrLn explanation

queryCommand :: Command (DebugQuery -> SqlM ())
queryCommand = SingleCommand CommandInfo
    { commandName = "query"
    , commandHeaderDesc = "query results"
    , commandDesc = "Show the query results."
    }
    $ pure debugQuery
  where
    debugQuery :: DebugQuery -> SqlM ()
    debugQuery (DebugQuery getQuery) = do
        query <- getQuery
        runSqlQueryConduit query $ C.mapM_ (liftIO . print)

timeQueryCommand :: Command (DebugQuery -> SqlM ())
timeQueryCommand = SingleCommand CommandInfo
    { commandName = "time"
    , commandHeaderDesc = "time query"
    , commandDesc = "Time how long it takes to run query."
    }
    $ pure timeQuery
  where
    timeQuery :: DebugQuery -> SqlM ()
    timeQuery (DebugQuery getQuery) = do
        query <- getQuery
        (timing, _) <- withTime $ runSqlQueryConduit query C.await
        liftIO . putStrLn $ "Query took: " ++ showGFloat (Just 3) timing "s"

toQuery :: [Text] -> Maybe (Query [PersistValue])
toQuery [] = Nothing
toQuery sqlLines = Just $ Query
    { queryName = "Interactive"
    , commonTableExpressions = []
    , params = []
    , convert = return
    , queryText = T.unlines sqlLines
    }

testQuery :: Input SqlM ()
testQuery = do
    mQuery <- toQuery <$> getManyInteractive textInput "SQL Query"
    case mQuery of
        Nothing -> return ()
        Just query -> do
            lift . runSqlQueryConduit query $ C.mapM_ printRow
            liftIO $ putStrLn ""
            testQuery
  where
    printRow :: MonadIO m => [PersistValue] -> m ()
    printRow = liftIO . putStrLn . renderSeparatedList " | " persistValueToString

    renderSeparatedList :: String -> (a -> String) -> [a] -> String
    renderSeparatedList sep f =
        concat . intersperse sep . map f

    renderPair :: (Text, PersistValue) -> String
    renderPair (name, val) = T.unpack name ++ " = " ++ persistValueToString val

    persistValueToString :: PersistValue -> String
    persistValueToString v = case v of
        PersistText txt -> T.unpack txt
        PersistByteString bs -> T.unpack $ encodeBase64 bs
        PersistInt64 i -> show i
        PersistDouble d -> show d
        PersistRational r -> show r
        PersistBool b -> show b
        PersistDay d -> show d
        PersistTimeOfDay time -> show time
        PersistUTCTime time -> show time
        PersistNull -> "NULL"
        PersistList l -> renderSeparatedList ", " persistValueToString l
        PersistMap m -> renderSeparatedList ", " renderPair m
        PersistObjectId bs -> T.unpack $ encodeBase64 bs
        PersistArray l -> renderSeparatedList ", " persistValueToString l
        PersistDbSpecific bs -> T.unpack $ encodeBase64 bs

explainQuery :: Input SqlM ()
explainQuery = do
    mQuery <- toQuery <$> getManyInteractive textInput "SQL Query"
    case mQuery of
        Nothing -> return ()
        Just query -> do
            explanation <- lift $ explainSqlQuery query
            liftIO $ T.putStrLn explanation
            explainQuery
