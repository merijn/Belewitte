{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Concurrent hiding (yield)
import Control.Monad.Catch (mask_)
import Control.Monad.IO.Class (MonadIO(liftIO))
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
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate.IsString
import Database.Persist.Sqlite
import GHC.Conc.Sync (getNumProcessors)
import Numeric (showFFloat)
import System.IO (hClose)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Posix.Types (Fd)

import qualified Data.Vector.Unboxed as VU

import Data.Binary.Put

import Model
import Query
import Schema

trainModel :: Query (Props, Algorithms) -> SqlM ByteString
trainModel query@Query{columnCount} = do
    numEntries <- runSqlQueryCount query

    let process :: Fd -> CreateProcess
        process fd = proc "./model.py"
            [ "--entries"
            , show numEntries
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

            runConduit $ runSqlQuery query .| combinedSink
            liftIO $ BS.hGetContents hnd

    withCheckedProcessCleanup (process outFd) handleStreams
  where
    putProps :: Props -> ByteString
    putProps = LBS.toStrict . runPut . VU.mapM_ putDoublehost

    putResults :: Algorithms -> ByteString
    putResults = LBS.toStrict . runPut . putInt64host

queryImplementations :: SqlM (IntMap Implementation)
queryImplementations = do
    Just (Entity aId _) <- runSql selectBfs
    runSql . runConduitRes $ selectImpls aId .| C.foldMap toIntMap
  where
    selectBfs = selectFirst [ AlgorithmName ==. "bfs" ] []

    selectImpls aId = selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

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
        input <- trainModel query
        let model = byteStringToModel input
            predictor = predict model
            aggregate (!right,!wrong) (x,y)
                | fromIntegral x == y = (right + 1, wrong)
                | otherwise = (right, wrong + 1)

        (right, wrong) <- runConduit $
            runSqlQuery (first predictor <$> query) .| C.foldl aggregate (0,0)

        liftIO $ do
            putStrLn $ "Right predictions: " ++ show (right :: Int)
            putStrLn $ "Wrong predictions: " ++ show wrong
            putStrLn $ "Error rate: " ++ percent wrong (right+wrong)

        impls <- queryImplementations
        dumpCppModel "test.cpp" model graphProps stepProps impls
  where
    percent :: Integral n => n -> n -> String
    percent x y = showFFloat (Just 2) val "%"
      where
        val :: Double
        val = 100 * fromIntegral x / fromIntegral y

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
