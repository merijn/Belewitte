{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad (forM_, replicateM)
import Control.Monad.Catch (mask_)
import Control.Monad.Trans.Resource (register, release)
import Data.Bifunctor (first)
import Data.Binary.Get (getDoublehost, getRemainingLazyByteString, runGet)
import Data.Binary.Put (putDoublehost, putInt64host, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Process
    (CreateProcess, Inherited(..), proc, withCheckedProcessCleanup)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (fromString)
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Database.Persist.Sqlite
import GHC.Conc.Sync (getNumProcessors, setNumCapabilities)
import Numeric (showFFloat)
import System.IO (hClose)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Posix.Types (Fd)

import Model
import Query
import Schema

trainModel :: Query (Vector Double, Int64) -> SqlM ([Double], Model)
trainModel query = do
    numEntries <- runSqlQueryCount query
    Just columnCount <- fmap (VS.length . fst) <$> runSqlQuery query C.head

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

                combinedSink = getZipSink (propertySink *> resultSink)

            runSqlQuery query combinedSink

            (importance, model) <- liftIO $
                getResult columnCount <$> BS.hGetContents hnd

            return (importance, byteStringToModel model)

    withCheckedProcessCleanup (process outFd) handleStreams
  where
    putProps :: Vector Double -> ByteString
    putProps = LBS.toStrict . runPut . VS.mapM_ putDoublehost

    putResults :: Int64 -> ByteString
    putResults = LBS.toStrict . runPut . putInt64host

    getResult :: Int -> ByteString -> ([Double], ByteString)
    getResult columnCount = runGet parseBlock . LBS.fromStrict
      where
        parseBlock = do
            dbls <- replicateM columnCount getDoublehost
            bs <- LBS.toStrict <$> getRemainingLazyByteString
            return (dbls, bs)

queryImplementations :: SqlM (IntMap Implementation)
queryImplementations = do
    Just (Entity aId _) <- selectBfs
    runConduitRes $ selectImpls aId .| C.foldMap toIntMap
  where
    selectBfs = selectFirst [ AlgorithmName ==. "bfs" ] []

    selectImpls aId = selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

dumpQueryToFile :: Show r => String -> Query r -> SqlM ()
dumpQueryToFile path query = runSqlQuery query $
    C.map ((++"\n") . show) .| C.map fromString .| C.sinkFile path

validateModel
    :: Model
    -> Query (Vector Double, Int64)
    -> Query (Vector Double, Int64)
    -> SqlM ()
validateModel model validation total = do
    computeResults "validation" validation
    computeResults "total" total
  where
    computeResults name query = do
        result <- runSqlQuery predictions $ C.foldl aggregate (0,0)
        report name result
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

reportFeatureImportance
    :: MonadIO m => [Double] -> Set Text -> Set Text -> m ()
reportFeatureImportance importances graphProps stepProps =
  forM_ featureList $ \(lbl, val) -> liftIO $ do
    putStrLn $ T.unpack lbl ++ ": " ++ showFFloat (Just 2) (val * 100) "%"
  where
    propList :: [Text]
    propList = S.toAscList graphProps ++ S.toAscList stepProps

    revCmpDouble :: (a, Double) -> (a, Double) -> Ordering
    revCmpDouble = flip (comparing snd)

    featureList :: [(Text, Double)]
    featureList = sortBy revCmpDouble $ zip propList importances

main :: IO ()
main = do
    getNumProcessors >>= setNumCapabilities

    runSqlM isNotDebug "test.db" $ do
        liftPersist $ runMigrationSilent migrateAll
        graphProps <- gatherProps "GraphProp"
        stepProps <- gatherProps "StepProp"
        let appendVectors (v1, v2, a) = (v1 <> v2, a)
            query = appendVectors <$> propertyQuery (toSqlKey 1) graphProps stepProps

        rowCount <- runSqlQueryCount query

        let trainingSize :: Integer
            trainingSize = round (fromIntegral rowCount * 0.6 :: Double)

            training, validation :: Query (Vector Double, Int64)
            (training, validation) = randomizeQuery 42 trainingSize query

        (importance, model) <- trainModel training

        reportFeatureImportance importance graphProps stepProps
        liftIO $ putStrLn ""
        validateModel model validation query

        impls <- queryImplementations
        dumpCppModel "test.cpp" model graphProps stepProps impls
  where
    gatherProps :: Text -> SqlM (Set Text)
    gatherProps table = runConduit $ rawQuery query [] .| C.foldMap toSet
      where
        toSet [PersistText t] = S.singleton t
        toSet _ = S.empty

        query = [i|SELECT DISTINCT property FROM #{table}|]

    isNotDebug :: LogSource -> LogLevel -> Bool
    isNotDebug _ LevelDebug = False
    isNotDebug _ _ = True
