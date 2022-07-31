{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module BarPlot
    ( BarPlotType(..)
    , BarPlot(..)
    , barPlot
    ) where

import Control.Monad (forM_)
import Control.Monad.Catch (handle)
import Data.Bifunctor (bimap, second)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (intersperse)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as Generic
import System.IO (Handle, hPutStr, stdout)

import ProcessTools (withStdin)

import Core
import GlobalPlotOptions
import RuntimeData (getBarPlotScript)
import Query (streamQuery)
import Query.Level (levelTimePlotQuery)
import Query.Time (timePlotQuery)
import Query.Variant (VariantInfo(..), VariantInfoConfig(..), variantInfoQuery)
import Schema
import Sql (Region)
import qualified Sql
import Utils.ImplTiming (ImplTiming(..))
import Utils.Pair (Pair(..), mergePair, toPair)

data BarPlotType
    = Levels
    | Totals { normalise :: Bool, useGraphId :: Bool, pdfName :: FilePath}
    | VsOptimal { normalise :: Bool, useGraphId :: Bool, pdfName :: FilePath }

data BarPlotConfig = BarPlotConfig
    { plotName :: FilePath
    , axisName :: String
    , slideFormat :: Bool
    , printStdout :: Bool
    , normaliseData :: Bool
    , numberedGroups :: Bool
    , rotateLabels :: Bool
    }

data BarPlot
    = BarPlot
    { barPlotGlobalOpts :: GlobalPlotOptions
    , barPlotType :: BarPlotType
    , barPlotSlideFormat :: Bool
    , barPlotPrintStdout :: Bool
    , barPlotRotateLabels :: Bool
    , barPlotNumberedGroups :: Bool
    , barPlotVariants :: Set (Key Variant)
    }

barPlot :: BarPlot -> SqlM ()
barPlot BarPlot{barPlotGlobalOpts = GlobalPlotOptions{..}, ..} = do
    case barPlotType of
        Levels -> forM_ barPlotVariants $ \variantId -> do
            graphId <- variantGraphId <$> Sql.getJust variantId
            name <- graphName <$> Sql.getJust graphId

            let plotName = T.unpack name <> "-levels.pdf"
                missingResults (PatternFailed _) = logErrorN $ mconcat
                    [ "Missing results for graph \"", name
                    , "\", variant #", showSqlKey variantId ]

            handle missingResults . runPlotScript (plotConfig plotName) $
                streamQuery (variantToLevelTimePlotQuery variantId)
                .| C.map (bimap showText (nameImplementations regular))

        Totals{useGraphId, pdfName} -> do
            let labelGraph
                    | useGraphId = showSqlKey . fst
                    | otherwise = snd

                translateData = translatePair . toPair V.convert V.convert

            runPlotScript (plotConfig pdfName) $
                streamQuery (variantsToTimePlotQuery barPlotVariants)
                .| C.map (bimap labelGraph translateData)

        VsOptimal{useGraphId, pdfName} -> runPlotScript (plotConfig pdfName) $
            streamQuery variantQuery
            .| C.filter variantFilter
            .| C.mapM (dataFromVariantInfo useGraphId)
            .| C.map (second $ translatePair . fmap V.convert)
  where
    plotConfig name = BarPlotConfig
        { plotName = name
        , slideFormat = barPlotSlideFormat
        , printStdout = barPlotPrintStdout
        , rotateLabels = barPlotRotateLabels
        , numberedGroups = barPlotNumberedGroups
        , ..
        }
      where
        (axisName, normaliseData) = case barPlotType of
            Levels -> ("Levels", False)
            Totals{normalise} -> ("Graph", normalise)
            VsOptimal{normalise} -> ("Graph", normalise)

    variantFilter VariantInfo{variantId} = S.member variantId barPlotVariants

    renamePair = Pair globalPlotImplRenames globalPlotImplRenames
    implMaps@Pair{regular} =
        IM.union <$> renamePair <*> toImplNames id id globalPlotImpls

    translatePair :: Pair (Vector ImplTiming) -> Vector (Text, Double)
    translatePair p = mergePair $ nameImplementations <$> implMaps <*> p

    variantsToTimePlotQuery =
        timePlotQuery globalPlotAlgorithm globalPlotPlatform globalPlotCommit

    variantToLevelTimePlotQuery =
        levelTimePlotQuery globalPlotPlatform globalPlotCommit

    variantQuery = variantInfoQuery $ VariantInfoConfig
        { variantInfoAlgorithm = globalPlotAlgorithm
        , variantInfoPlatform = globalPlotPlatform
        , variantInfoCommit = globalPlotCommit
        , variantInfoVariantConfigs = Nothing
        , variantInfoDatasets = Nothing
        , variantInfoTimestamp = globalPlotTimestamp
        , variantInfoAllowNewer = globalPlotAllowNewer
        , variantInfoFilterIncomplete = False
        }

dataFromVariantInfo
    :: Bool -> VariantInfo -> Region SqlM (Text, Pair (Vector ImplTiming))
dataFromVariantInfo useGraphId VariantInfo{..} = do
    graphId <- variantGraphId <$> Sql.getJust variantId
    name <- labelGraph graphId
    return (name, Pair extendedTimings (V.convert variantExternalTimings))
  where
    labelGraph graphId
        | useGraphId = return $ showSqlKey graphId
        | otherwise = graphName <$> Sql.getJust graphId

    extendedTimings = V.convert variantTimings
        `V.snoc` ImplTiming bestNonSwitchingImplId variantBestNonSwitching
        `V.snoc` ImplTiming optimalImplId variantOptimal

nameImplementations
    :: Generic.Vector v ImplTiming
    => IntMap Text
    -> v ImplTiming
    -> Vector (Text, Double)
nameImplementations impls = V.mapMaybe translate . V.convert
  where
    translate :: ImplTiming -> Maybe (Text, Double)
    translate (ImplTiming impl val) = (,val) <$> impls IM.!? fromIntegral impl

runPlotScript
    :: BarPlotConfig
    -> ConduitT () (Text, Vector (Text, Double)) (Region SqlM) ()
    -> SqlM ()
runPlotScript BarPlotConfig{..} queryDataConduit
  | printStdout = doWithHandle stdout
  | otherwise = do
        plotProcess <- getBarPlotScript args
        withStdin plotProcess doWithHandle
  where
    args :: [String]
    args = [ plotName
           , axisName
           , show normaliseData
           , show slideFormat
           , show rotateLabels
           , show numberedGroups
           ]

    doWithHandle :: Handle -> SqlM ()
    doWithHandle hnd = Sql.runRegionConduit $
        queryDataConduit .| do
            impls <- C.peek >>= \case
                Just (_, v) -> return $ V.map fst v
                Nothing -> logThrowM . PatternFailed $
                    "Expected at least one output row"

            liftIO . T.hPutStrLn hnd $ toColumnLabels impls
            C.mapM_ $ printGraph hnd impls

    toColumnLabels :: Vector Text -> Text
    toColumnLabels = mconcat . intersperse ":" . V.toList

    printGraph
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => Handle -> Vector Text -> (Text, Vector (Text, Double)) -> m ()
    printGraph hnd impls (graph, timingsPair)
        | mismatch = logThrowM $
            QueryResultMismatch (V.toList impls) (V.toList timingImpls)

        | otherwise = liftIO $ do
            T.hPutStr hnd $ graph <> " :"
            V.forM_ processedTimings $ \time ->
                hPutStr hnd $ " " ++ show time
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
            | normaliseData = V.map (/ maxTime) timings
            | otherwise = timings
