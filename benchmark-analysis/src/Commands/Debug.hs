{-# OPTIONS_GHC -Wno-deprecations #-}
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
import Text.Read (readMaybe)

import Commands
import Core
import FormattedOutput (renderOutput, renderRegionOutput)
import InteractiveInput
import Migration (MigrationSafety(..))
import OptionParsers
import Query (Converter(Simple), Query(..))
import qualified Query
import Query.Variant (VariantInfo, variantInfoQuery)
import Schema (PersistValue(..), schemaVersion)

data DebugQuery where
    DebugQuery :: Show v => SqlM (Query v) -> DebugQuery

commands
    :: (FilePath -> SqlM ())
    -> Map String (Parser DebugQuery)
    -> Command (WorkInput a)
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
            $ BuiltInCommand . dumpCommand <$> suffixParser
        , SingleCommand CommandInfo
            { commandName = "migrate"
            , commandHeaderDesc = "migrate to a specific schema version"
            , commandDesc =
                "Migrate the database to a specific version of the schema"
            } migrationParser
        , CommandGroup CommandInfo
            { commandName = "interactive"
            , commandHeaderDesc = "interactive queries from commandline"
            , commandDesc = "Interactively read queries from commandline"
            }
            $ map (fmap BuiltInCommand)
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
        ] ++ map (fmap BuiltInCommand) (buildQueryList debugQueryCommand)

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

migrationParser :: Parser (WorkInput a)
migrationParser = ManualMigration <$> safetyFlag <*> targetVersion
  where
    safetyFlag :: Parser MigrationSafety
    safetyFlag = flag MigrateSafe MigrateUnsafe $ mconcat
        [ long "unsafe", help "Perform migration without checking foreign key\
        \ constraints first."
        ]

    targetVersion :: Parser Int64
    targetVersion = argument (eitherReader idReader) $ mconcat
        [ metavar "ID", help "Version to migrate the database to." ]
      where
        idReader :: String -> Either String Int64
        idReader s = case readMaybe s of
            Nothing -> Left "Schema version must be numeric."
            Just i -> checkVersion i

        checkVersion :: Int64 -> Either String Int64
        checkVersion i | i < 0 = Left $ mconcat
            [ "Schema version must be between 0 and ", show schemaVersion ]

        checkVersion i | i > schemaVersion = Left $ mconcat
            [ "Target version ", show i
            , " is newer than latest schema version ", show schemaVersion
            ]

        checkVersion i = Right i

debugQueryCommand :: String -> Parser DebugQuery -> Command (SqlM ())
debugQueryCommand name flags = CommandGroupWithFlags CommandInfo
    { commandName = name
    , commandHeaderDesc = "debug commands for: " ++ name
    , commandDesc = "Debug commands for: " ++ name
    }
    flags
    [ logQueryCommand, explainQueryCommand, countQueryCommand, queryCommand
    , timeQueryCommand
    ]

logQueryCommand :: Command (DebugQuery -> SqlM ())
logQueryCommand = SingleCommand CommandInfo
    { commandName = "log"
    , commandHeaderDesc = "show query"
    , commandDesc = "Show the query"
    }
    $ pure logDebugQuery
  where
    logDebugQuery :: DebugQuery -> SqlM ()
    logDebugQuery (DebugQuery getQuery) = do
        query <- getQuery
        renderOutput . C.yield $ Query.toQueryText query

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
        (startTiming, _) <- withTime $
            Query.runSqlQueryConduit query C.await

        liftIO . putStrLn $
            "First row took: " ++ showGFloat (Just 3) startTiming "s\n"

        (totalTiming, _) <- withTime $
            Query.runSqlQueryConduit query C.sinkNull

        liftIO . putStrLn $
            "Query took: " ++ showGFloat (Just 3) totalTiming "s\n"

toQuery :: [Text] -> Maybe (Query [PersistValue])
toQuery [] = Nothing
toQuery sqlLines = Just $ Query
    { queryName = "Interactive"
    , commonTableExpressions = []
    , params = []
    , convert = Simple return
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
        PersistLiteral bs -> encodeBase64 bs
        PersistLiteralEscaped bs -> encodeBase64 bs

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
                renderOutput $ C.yield explanation
            liftIO $ putStrLn ""
            explainQuery

timeQuery :: Input SqlM ()
timeQuery = do
    mQuery <- toQuery <$> getManyInteractive textInput "SQL Query"
    case mQuery of
        Nothing -> return ()
        Just query -> do
            (startTiming, _) <- lift . withTime $
                Query.runSqlQueryConduit query C.await

            liftIO . putStrLn $
                "First row took: " ++ showGFloat (Just 3) startTiming "s\n"

            (totalTiming, _) <- lift . withTime $
                Query.runSqlQueryConduit query C.sinkNull

            liftIO . putStrLn $
                "Query took: " ++ showGFloat (Just 3) totalTiming "s\n"

            timeQuery
