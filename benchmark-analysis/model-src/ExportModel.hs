{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Concurrent hiding (yield)
import Control.Monad (forM_, replicateM)
import Control.Monad.Catch (mask_)
import Control.Monad.Logger
    ( LogSource, LogLevel(..), filterLogger, runChanLoggingT
    , runStderrLoggingT, unChanLoggingT)
import Control.Monad.Trans.Resource
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Process
    (CreateProcess, Inherited(..), proc, withCheckedProcessCleanup)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (fromString)
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Database.Persist.Sqlite
import GHC.Conc.Sync (getNumProcessors)
import Numeric (showFFloat)
import System.IO (hClose)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Posix.Types (Fd)

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU

import Data.Binary.Get
import Data.Binary.Put

import Model
import Query
import Schema

trainModel
    :: Double
    -> Key GPU
    -> Set Text
    -> Set Text
    -> SqlTx (Map Text Double, Model)
trainModel fraction gpuId graphProps stepProps = do
    numEntries <- runSqlQueryCount query

    let trainingSize :: Integer
        trainingSize = round (fraction * fromIntegral numEntries)

    let process :: Fd -> CreateProcess
        process fd = proc "./model.py"
            [ "--entries"
            , show trainingSize
            , "--properties"
            , show columnCount
            , "--fd"
            , show fd
            ]

    ((outFd, closeOut), (resultsHnd, closeResults)) <- mask_ $ do
        (fdOut, fdIn) <- liftIO createPipe
        hndIn <- liftIO $ fdToHandle fdIn
        closeIn <- register $ hClose hndIn
        closeOut <- register $ closeFd fdOut
        return ((fdOut, closeOut), (hndIn, closeIn))

    let handleStreams (propSink, closeSink) hnd Inherited = do
            register $ hClose hnd
            release closeOut
            processIn <- register closeSink

            let propertySink = ZipSink $ do
                    C.map (putProps . fst) .| propSink
                    release processIn

                resultSink = ZipSink $ do
                    C.map (putResults . snd) .| C.sinkHandle resultsHnd
                    release closeResults

                combinedSink = getZipSink (propertySink <* resultSink)

            runConduit $ runSqlQueryRandom fraction query .| combinedSink
            (importance, model) <- liftIO $ getResult <$> BS.hGetContents hnd
            return (importanceMap importance, byteStringToModel model)

    withCheckedProcessCleanup (process outFd) handleStreams
  where
    query@Query{columnCount} = propertyQuery gpuId graphProps stepProps

    propList :: [Text]
    propList = S.toAscList graphProps ++ S.toAscList stepProps

    importanceMap :: [Double] -> Map Text Double
    importanceMap = M.fromList . zip propList

    putProps :: Props -> ByteString
    putProps = LBS.toStrict . runPut . VU.mapM_ putDoublehost

    putResults :: Algorithms -> ByteString
    putResults = LBS.toStrict . runPut . putInt64host

    getResult :: ByteString -> ([Double], ByteString)
    getResult = runGet parseBlock . LBS.fromStrict
      where
        parseBlock = do
            dbls <- replicateM columnCount getDoublehost
            bs <- LBS.toStrict <$> getRemainingLazyByteString
            return (dbls, bs)

queryImplementations :: SqlM (IntMap Implementation)
queryImplementations = do
    Just (Entity aId _) <- runSql selectBfs
    runSql . runConduitRes $ selectImpls aId .| C.foldMap toIntMap
  where
    selectBfs = selectFirst [ AlgorithmName ==. "bfs" ] []

    selectImpls aId = selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

dumpQueryToFile
    :: (Show (v Double), V.Vector v Double)
    => String
    -> Query (v Double, Algorithms)
    -> SqlM ()
dumpQueryToFile path query = runSql . runConduit $
    runSqlQuery query
        .| C.map ((++"\n") . show)
        .| C.map fromString
        .| C.sinkFile path

validateModel
    :: V.Vector v Double => Model -> Query (v Double, Algorithms) -> SqlM ()
validateModel model query = do
    result <- runSql . runConduit $ runSqlQuery predictions .| C.foldl aggregate (0,0)
    report "total" result
  where
    predictions = first (predict model) <$> query

    aggregate (!right,!wrong) (x,y)
        | fromIntegral x == y = (right + 1, wrong)
        | otherwise = (right, wrong + 1)

    report :: MonadIO m => String -> (Int, Int) -> m ()
    report name (right, wrong) = liftIO . putStrLn . unlines $
        [ "Right predictions (" ++ name ++ "): " ++ show right
        , "Wrong predictions (" ++ name ++ "): " ++ show wrong
        , "Error rate (" ++ name ++ "): " ++ percent wrong (right+wrong)
        ]
      where
        percent :: Integral n => n -> n -> String
        percent x y = showFFloat (Just 2) val "%"
          where
            val :: Double
            val = 100 * fromIntegral x / fromIntegral y

reportFeatureImportance :: MonadIO m => Map Text Double -> m ()
reportFeatureImportance importances =
  forM_ featureList $ \(lbl, val) -> liftIO $ do
    putStrLn $ T.unpack lbl ++ ": " ++ showFFloat (Just 2) (val * 100) "%"
  where
    revCmpDouble :: (a, Double) -> (a, Double) -> Ordering
    revCmpDouble = flip (comparing snd)

    featureList :: [(Text, Double)]
    featureList = sortBy revCmpDouble $ M.toList importances

main :: IO ()
main = do
    numCPUs <- getNumProcessors
    setNumCapabilities numCPUs

    logChan <- newChan
    forkIO . runStderrLoggingT . filterLogger isNotDebug $
        unChanLoggingT logChan

    runChanLoggingT logChan . runWithDb "test.db" numCPUs $ do
        runSql $ runMigrationSilent migrateAll
        graphProps <- gatherProps "GraphProp"
        stepProps <- gatherProps "StepProp"
        let query = propertyQuery (toSqlKey 1) graphProps stepProps

        (importance, model) <- runSql $ trainModel 0.6 (toSqlKey 1) graphProps stepProps
        reportFeatureImportance importance
        liftIO $ putStrLn ""
        validateModel model query

        impls <- queryImplementations
        dumpCppModel "test.cpp" model graphProps stepProps impls
  where
    gatherProps :: Text -> SqlM (Set Text)
    gatherProps table =
        runSql . runConduit $ rawQuery query [] .| C.foldMap toSet
      where
        toSet [PersistText t] = S.singleton t
        toSet _ = S.empty

        query = [i|SELECT DISTINCT property FROM #{table}|]

    isNotDebug :: LogSource -> LogLevel -> Bool
    isNotDebug _ LevelDebug = False
    isNotDebug _ _ = True

    runWithDb :: Text -> Int -> SqlM a -> LoggingT IO a
    runWithDb path n = runResourceT . withSqlitePool path n . runReaderT
