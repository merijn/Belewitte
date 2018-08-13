{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import Control.Monad ((>=>), forM_, guard)
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sqlite (Key, Entity(..), (==.))
import qualified Database.Persist.Sqlite as Sql
import System.Directory (doesFileExist)
import System.FilePath (splitExtension, takeFileName)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import BroadcastChan.Conduit
import Core
import Jobs
import OptionParsers
import Parsers
import ProcessPool
import Schema

runTask :: Process -> (a, b, c, Text) -> IO (a, b, c, Text)
runTask Process{..} (x, y, z, cmd) = do
    -- FIXME proper logging
    T.putStrLn $ "Task: " <> cmd
    T.hPutStrLn inHandle cmd
    result <- T.hGetLine outHandle
    T.putStrLn $ "Done: " <> cmd
    return (x, y, z, result)

queryLineWith :: MonadIO m => (Text -> Maybe a) -> Text -> m (Maybe a)
queryLineWith convert prompt = liftIO $ do
    T.putStr $ prompt <> ": "
    hFlush stdout
    (checkEmpty >=> convert) <$> T.getLine
  where
    checkEmpty :: Text -> Maybe Text
    checkEmpty t = if T.null t then Nothing else Just t

queryLine :: MonadIO m => Text -> m (Maybe Text)
queryLine = queryLineWith return

queryRead :: (MonadIO m, Read a) => Text -> m (Maybe a)
queryRead = queryLineWith (readMaybe . T.unpack)

addGPU :: SqlM ()
addGPU = do
    Just gpuName <- queryLine "GPU Slurm Name"
    prettyName <- queryLine "GPU Pretty Name"
    Sql.insert_ $ GPU gpuName prettyName

addGraphs :: [FilePath] -> SqlM ()
addGraphs = mapM_ $ \path -> do
    liftIO $ doesFileExist path >>= guard
    let (graphName, ext) = splitExtension $ takeFileName path
    liftIO $ guard (ext == ".graph")
    Sql.insert_ $ Graph (T.pack graphName) (T.pack path) Nothing

addAlgorithm :: SqlM ()
addAlgorithm = do
    Just algoName <- queryLine "Algorithm Name"
    prettyName <- queryLine "Algorithm Pretty Name"
    Sql.insert_ $ Algorithm algoName prettyName

addImplementation :: SqlM ()
addImplementation = do
    Just algoName <- queryLine "Algorithm Name"
    Just (Entity algoId _) <- Sql.getBy (UniqAlgorithm algoName)
    Just implName <- queryLine "Implementation Name"
    prettyName <- queryLine "Implementation Pretty Name"
    flags <- queryLine "Flags"
    Just algoType <- queryRead "Algorithm type"
    Just runnable <- queryRead "Runnable"
    Sql.insert_ $
        Implementation algoId implName prettyName flags algoType runnable

addVariant :: SqlM ()
addVariant = do
    Just algoName <- queryLine "Algorithm Name"
    Just (Entity algoId _) <- Sql.getBy (UniqAlgorithm algoName)
    Just variantName <- queryLine "Variant Name"
    flags <- queryLine "Flags"
    let mkVariant gId = Variant gId algoId variantName flags Nothing
    runConduit $
        Sql.selectKeys [] []
        .| C.mapM_ (Sql.insert_ . mkVariant)

importResults :: SqlM ()
importResults = do
    Just gpuName <- queryLine "GPU Name"
    Just (Entity gpuId _) <- Sql.getBy (UniqGPU gpuName)
    Just algoName <- queryLine "Algorithm Name"
    Just (Entity algoId _) <- Sql.getBy (UniqAlgorithm algoName)
    Just implName <- queryLine "Implementation Name"
    Just (Entity implId _) <- Sql.getBy (UniqImpl algoId implName)
    Just filepath <- queryLine "Result file"
    runConduit $
        C.sourceFile (T.unpack filepath)
        .| C.decode C.utf8
        .| C.map (T.replace "," "")
        .| conduitParse externalResult
        .| C.mapM_ (insertResult gpuId algoId implId)
  where
    insertResult
        :: Key GPU
        -> Key Algorithm
        -> Key Implementation
        -> ExternalResult
        -> SqlM ()
    insertResult gpuId algoId implId (Result gname variantName Timing{..}) = do
        [graphId] <- logIfFail "More than one graph found for" gname $
            Sql.selectKeysList [GraphName ==. gname] []

        let uniqVariant = UniqVariant graphId algoId varName

        Just varId <- logIfFail "No variant found" varName $
            fmap entityKey <$> Sql.getBy uniqVariant

        Sql.insert_ $
            TotalTimer gpuId varId implId name minTime avgTime maxTime stddev
      where
        --FIXME get from command
        varName
            | variantName == "0" = "default"
            | otherwise = "Root " <> variantName

runBenchmarks :: Int -> Int -> SqlM ()
runBenchmarks numNodes numRuns = do
    -- FIXME hardcoded GPU
    withProcessPool numNodes (GPU "TitanX" Nothing) $ \procPool -> runConduit $
        propertyJobs
        .| parMapM (Simple Retry) numNodes (withProcess procPool runTask)
        .| C.mapM_ processProperty

    gpus <- Sql.selectList [] []
    forM_ gpus $ \(Entity gpuId gpu) -> do
        withProcessPool numNodes gpu $ \procPool -> runConduit $
            timingJobs numRuns gpuId
            .| parMapM (Simple Retry) numNodes (withProcess procPool runTask)
            .| C.mapM_ (processTiming gpuId)

commands :: String -> (InfoMod a, Parser (SqlM ()))
commands name = (,) docs . hsubparser $ mconcat
    [ subCommand "add-gpu" "register a new GPU" "" $
        pure addGPU
    , subCommand "add-graphs" "register graphs" "" $
        addGraphs <$> some (strArgument graphFile)
    , subCommand "add-algorithm" "register a new algorithm" "" $
        pure addAlgorithm
    , subCommand "add-implementation"
                 "register a new implementation of an algorithm" "" $
        pure addImplementation
    , subCommand "add-variant" "register a new variant of a graph" "" $
        pure addVariant
    , subCommand "import-results" "import results of external tools" "" $
        pure importResults
    , subCommand "run-benchmarks" "run benchmarks"
                 "Run benchmarks for missing registered configurations" $
                 runBenchmarks <$> parallelism <*> numRuns
    ]
  where
    docs = mconcat
      [ fullDesc
      , header $ name ++ " - a tool for registering and running GPU benchmarks"
      , progDesc
        "Register GPUs, algorithms, algorithm implementations, and graphs in \
        \an SQLite database of configurations. Automatically run missing \
        \configurations and store all results in the database."
      ]

    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ name ++ " " ++ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

    graphFile = metavar "GRAPH" <> help "Graph file"
    parallelism = option auto . mconcat $ [ metavar "N", short 'j', value 2 ]
    numRuns = option auto . mconcat $ [ metavar "N", short 'n', value 30 ]


main :: IO ()
main = runSqlM commands id
