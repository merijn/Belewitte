{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module BarPlot (barPlot) where

import Data.Conduit (ConduitT, Void, (.|))
import qualified Data.Conduit.Combinators as C
import Data.List (intersperse)
import Data.Vector (Vector)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.IO (Handle, hPutStr, stdout)

import Core
import RuntimeData (getBarPlotScript)
import Sql (Region, runRegionConduit)
import Utils.Process (withStdin)

import PlotOptions

barPlot
    :: PlotConfig
    -> Text
    -> ConduitT () (Text, Vector (Text, Double)) (Region SqlM) ()
    -> SqlM ()
barPlot PlotConfig{..} plotName queryDataConduit
  | printStdout = doWithHandle stdout
  | otherwise = do
        plotProcess <- getBarPlotScript args
        withStdin plotProcess doWithHandle
  where
    args :: [String]
    args = [T.unpack plotName, axisName, show normalise, show slideFormat]

    doWithHandle :: Handle -> SqlM ()
    doWithHandle hnd = runRegionConduit $
        queryDataConduit .| reportData hnd normalise

reportData
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => Handle -> Bool -> ConduitT (Text, Vector (Text, Double)) Void m ()
reportData hnd normalise = do
    impls <- C.peek >>= \case
        Just (_, v) -> return $ V.map fst v
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one output row"

    liftIO . T.hPutStrLn hnd $ toColumnLabels impls
    C.mapM_ $ printGraph impls
  where
    toColumnLabels :: Vector Text -> Text
    toColumnLabels = mconcat . intersperse ":" . V.toList

    printGraph
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => Vector Text -> (Text, Vector (Text, Double)) -> m ()
    printGraph impls (graph, timingsPair)
        | mismatch = logThrowM $
            QueryResultMismatch (V.toList impls) (V.toList timingImpls)

        | otherwise = liftIO $ do
            T.hPutStr hnd $ graph <> " :"
            V.forM_ processedTimings $ \time -> hPutStr hnd $ " " ++ show time
            T.hPutStrLn hnd ""
      where
        mismatch :: Bool
        mismatch = not . V.and $ V.zipWith (==) impls timingImpls

        timingImpls :: Vector Text
        timings :: Vector Double
        (timingImpls, timings) = V.unzip timingsPair

        maxTime :: Double
        maxTime = V.maximum . V.filter (not . isInfinite) $ timings

        processedTimings :: Vector Double
        processedTimings
            | normalise = V.map (/ maxTime) timings
            | otherwise = timings

