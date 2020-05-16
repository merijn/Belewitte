{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Heatmap
    ( VariantSelection(..)
    , Heatmap(..)
    , plotHeatmap
    ) where

import Data.Binary.Put (putDoublehost, runPut)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.IO as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import System.IO (hClose)

import Core
import Query
import RuntimeData (getHeatmapScript)
import Schema
import Sql (runRegionConduit)
import Utils.ImplTiming
import Utils.Pair (Pair(..))
import Utils.Process (Inherited(..), ReadWrite(Write))
import qualified Utils.Process as Proc
import VariantQuery

import GlobalPlotOptions

data VariantSelection = ConfigSelection (Key VariantConfig) | Everything

data Heatmap
    = TotalHeatmap
      { heatmapGlobalOpts :: GlobalPlotOptions
      , heatmapVariantSelection :: VariantSelection
      , heatmapDataset :: Maybe (Key Dataset)
      , heatmapShowOptimal :: Bool
      }

plotHeatmap :: Heatmap -> SqlM ()
plotHeatmap TotalHeatmap{heatmapGlobalOpts = GlobalPlotOptions{..}, ..} = do
    numVariants <- runSqlQueryCount variantQuery
    runPlotScript implNames numVariants $ streamQuery variantQuery
  where
    Pair implNames _ = toImplNames id id globalPlotImpls

    extractTimings :: VariantInfo -> Vector ImplTiming
    extractTimings VariantInfo{..}
        | heatmapShowOptimal = extendTimings variantTimings
        | otherwise = variantTimings
      where
        extendTimings = VS.cons (ImplTiming optimalImplId variantOptimal)

    variantQuery = extractTimings <$> variantInfoQuery VariantInfoConfig
        { variantInfoAlgorithm = globalPlotAlgorithm
        , variantInfoPlatform = globalPlotPlatform
        , variantInfoCommit = globalPlotCommit
        , variantInfoVariantConfig = case heatmapVariantSelection of
            Everything -> Nothing
            ConfigSelection n -> Just n
        , variantInfoDataset = heatmapDataset
        , variantInfoFilterIncomplete = False
        }

runPlotScript
    :: IntMap Text
    -> Int
    -> ConduitT () (Vector ImplTiming) (Region SqlM) ()
    -> SqlM ()
runPlotScript implNames rowCount queryDataConduit = do
    (implCount, implVec) <- runRegionConduit $ queryDataConduit .| do
        firstVec <- C.head
        case firstVec of
            Just v -> return (VS.length v, v)
            Nothing -> logThrowM . PatternFailed $
                "Unable to compute implementation count for results"

    Proc.runProcessCreation_ $ do
        (dataFd, dataHnd) <- Proc.withPipe Write
        plotProcess <- lift $
            getHeatmapScript [dataFd, show rowCount, show implCount, "out"]

        Proc.withProcess plotProcess $ \columnHnd Inherited -> do
            VS.forM_ implVec $ liftIO . T.hPutStrLn columnHnd . lookupName

            liftIO $ hClose columnHnd

            runRegionConduit $
                queryDataConduit
                .| C.map (putTimings . normaliseVector)
                .| C.sinkHandle dataHnd

            liftIO $ hClose dataHnd
  where
    lookupName :: ImplTiming -> Text
    lookupName ImplTiming{implTimingImpl} =
        IM.findWithDefault "?" (fromIntegral implTimingImpl) implNames

normaliseVector :: VS.Vector ImplTiming -> VS.Vector Double
normaliseVector vec = VS.map (/ VS.maximum newVec) newVec
  where
    newVec = VS.map implTimingTiming vec

putTimings :: VS.Vector Double -> ByteString
putTimings = LBS.toStrict . runPut . VS.mapM_ putDoublehost
