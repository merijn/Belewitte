{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Debug (DebugQuery(..), commands) where

import Data.ByteString.Base64 (encodeBase64)
import Data.Char (isSpace)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Numeric (showGFloat)

import Commands
import Core
import FormattedOutput (renderOutput, renderRegionOutput)
import InteractiveInput
import OptionParsers
import Query (Query(..))
import qualified Query
import Schema (PersistValue(..))
import VariantQuery (VariantInfo, variantInfoQuery)

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
                { commandName = "count"
                , commandHeaderDesc = "print query result count to stdout"
                , commandDesc =
                  "Read query from commandline and print result count to stdout"
                }
                $ pure (runInput countQuery)
            , SingleCommand CommandInfo
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
            , SingleCommand CommandInfo
                { commandName = "time"
                , commandHeaderDesc = "print query time to stdout"
                , commandDesc =
                  "Read query from commandline and print the time it takes \
                  \to stdout"
                }
                $ pure (runInput timeQuery)
            ]
        ] ++ buildQueryList debugQueryCommand

    completeQueryMap :: Map String (Parser DebugQuery)
    completeQueryMap =
      M.insert "variantInfoQuery" (DebugQuery <$> variantQuery) queryMap

    variantQuery :: Parser (SqlM (Query VariantInfo))
    variantQuery = fmap variantInfoQuery <$> variantInfoConfigParser

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
    [explainQueryCommand, countQueryCommand, queryCommand, timeQueryCommand]

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
        explanation <- Query.explainSqlQuery query
        renderOutput $ C.yield explanation

countQueryCommand :: Command (DebugQuery -> SqlM ())
countQueryCommand = SingleCommand CommandInfo
    { commandName = "count"
    , commandHeaderDesc = "count results"
    , commandDesc = "Show the query result count."
    }
    $ pure debugQuery
  where
    debugQuery :: DebugQuery -> SqlM ()
    debugQuery (DebugQuery getQuery) = do
        query <- getQuery
        Query.runSqlQueryCount query >>= liftIO . print

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
        renderRegionOutput $
            Query.streamQuery query .| C.map showText .| C.unlines

timeQueryCommand :: Command (DebugQuery -> SqlM ())
timeQueryCommand = SingleCommand CommandInfo
    { commandName = "time"
    , commandHeaderDesc = "time query"
    , commandDesc = "Time how long it takes to run query."
    }
    $ pure debugQuery
  where
    debugQuery :: DebugQuery -> SqlM ()
    debugQuery (DebugQuery getQuery) = do
        query <- getQuery
        (timing, _) <- withTime $ Query.runSqlQueryConduit query C.await
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
            lift . renderRegionOutput $
                Query.streamQuery query .| C.map renderRow .| C.unlines
            liftIO $ putStrLn ""
            testQuery
  where
    renderRow :: [PersistValue] -> Text
    renderRow = renderSeparatedList " | " persistValueToText

    renderSeparatedList :: Text -> (a -> Text) -> [a] -> Text
    renderSeparatedList sep f = T.intercalate sep . map f

    renderPair :: (Text, PersistValue) -> Text
    renderPair (name, val) = name <> " = " <> persistValueToText val

    persistValueToText :: PersistValue -> Text
    persistValueToText v = case v of
        PersistText txt -> txt
        PersistByteString bs -> encodeBase64 bs
        PersistInt64 i -> showText i
        PersistDouble d -> showText d
        PersistRational r -> showText r
        PersistBool b -> showText b
        PersistDay d -> showText d
        PersistTimeOfDay time -> showText time
        PersistUTCTime time -> showText time
        PersistNull -> "NULL"
        PersistList l -> renderSeparatedList ", " persistValueToText l
        PersistMap m -> renderSeparatedList ", " renderPair m
        PersistObjectId bs -> encodeBase64 bs
        PersistArray l -> renderSeparatedList ", " persistValueToText l
        PersistDbSpecific bs -> encodeBase64 bs

countQuery :: Input SqlM ()
countQuery = do
    mQuery <- toQuery <$> getManyInteractive textInput "SQL Query"
    case mQuery of
        Nothing -> return ()
        Just query -> do
            lift $ Query.runSqlQueryCount query >>= liftIO . print
            liftIO $ putStrLn ""
            countQuery

explainQuery :: Input SqlM ()
explainQuery = do
    mQuery <- toQuery <$> getManyInteractive textInput "SQL Query"
    case mQuery of
        Nothing -> return ()
        Just query -> do
            lift $ do
                explanation <- Query.explainSqlQuery query
                renderOutput $ do
                    C.yield explanation
                    C.yield "\n"
            explainQuery

timeQuery :: Input SqlM ()
timeQuery = do
    mQuery <- toQuery <$> getManyInteractive textInput "SQL Query"
    case mQuery of
        Nothing -> return ()
        Just query -> do
            (timing, _) <- lift . withTime $
                Query.runSqlQueryConduit query C.await
            liftIO . putStrLn $
                "Query took: " ++ showGFloat (Just 3) timing "s\n"
            timeQuery
