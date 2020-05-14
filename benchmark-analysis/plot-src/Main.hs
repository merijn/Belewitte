{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Monad (forM, forM_)
import Control.Monad.Catch (handle)
import Data.Bifunctor (bimap, second)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (catMaybes)
import Data.Monoid (Any(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as Generic

import Core
import BarPlot (barPlot)
import Options
import LevelQuery (levelTimePlotQuery)
import PlotOptions (PlotOptions(..), PlotType(..), commands)
import Query
import Schema
import Sql ((==.))
import qualified Sql
import TimeQuery (timePlotQuery)
import Utils.ImplTiming
import Utils.Pair (Pair(..), toPair, mergePair)
import VariantQuery

queryVariants :: Key Algorithm -> Set Text -> SqlM (Set (Key Variant))
queryVariants algoId graphs = do
    gids <- Sql.selectSource [] [] $
        C.filter (\i -> S.member (graphName (Sql.entityVal i)) graphs)
        .| C.map Sql.entityKey
        .| C.foldMap S.singleton

    variantConfigId <- Sql.selectKeysList
        [VariantConfigAlgorithmId ==. algoId, VariantConfigIsDefault ==. True]
        [] >>= \case
            [key] -> return key
            [] -> logThrowM . PatternFailed $
              "No default variant config for algorithm #" <> showSqlKey algoId
            _ -> logThrowM . PatternFailed . mconcat $
                [ "Multiple default variant configs for algorithm #"
                , showSqlKey algoId ]

    variants <- forM (S.toList gids) $ \gId ->
        Sql.getBy $ UniqVariant gId variantConfigId

    return . S.fromList . map Sql.entityKey . catMaybes $ variants

filterImpls :: Set Text -> IntMap Implementation -> IntMap Implementation
filterImpls textSet = IM.filter $ getAny . mconcat
    [ Any . (`S.member` textSet) . implementationName
    , Any . (Builtin==) . implementationType
    ]

filterExternal :: Set Text -> IntMap ExternalImpl -> IntMap ExternalImpl
filterExternal names = IM.filter ((`S.member` names) . externalImplName)

dataFromVariantInfo
    :: VariantInfo -> Region SqlM (Text, Pair (Vector ImplTiming))
dataFromVariantInfo VariantInfo{..} = do
    graphId <- variantGraphId <$> Sql.getJust variantId
    name <- graphName <$> Sql.getJust graphId
    return (name, Pair extendedTimings (V.convert variantExternalTimings))
  where
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

main :: IO ()
main = runSqlMCommand commands $ \PlotOptions{..} -> do
    variants <- queryVariants algorithmId graphSet

    impls <- (,) <$> Sql.queryImplementations algorithmId
                 <*> Sql.queryExternalImplementations algorithmId

    let filteredImpls = filterImpls implementationNames
        filteredExt = filterExternal implementationNames
        implMaps@Pair{regular} = toImplNames filteredImpls filteredExt impls

        translatePair :: Pair (Vector ImplTiming) -> Vector (Text, Double)
        translatePair x = mergePair $ nameImplementations <$> implMaps <*> x

        timeQuery = timePlotQuery algorithmId platformId commitId variants

        variantQuery = variantInfoQuery $
            VariantInfoConfig algorithmId platformId commitId Nothing Nothing False

        variantFilter VariantInfo{variantId} = S.member variantId variants

    case plotType of
        PlotLevels -> do
            forM_ variants $ \variantId -> do
                graphId <- variantGraphId <$> Sql.getJust variantId
                name <- graphName <$> Sql.getJust graphId

                let query = levelTimePlotQuery platformId commitId variantId
                    pdfName = name <> "-levels"
                    missingResults (PatternFailed _) = logErrorN $ mconcat
                        [ "Missing results for graph \"", name
                        , "\", variant #", showSqlKey variantId ]

                handle missingResults . plot plotConfig pdfName $
                    streamQuery query
                    .| C.map (bimap showText (nameImplementations regular))

        PlotTotals -> do
            plot plotConfig "times-totals" $
                streamQuery timeQuery
                .| C.map (second $ translatePair . toPair V.convert V.convert)

        PlotVsOptimal -> do
            plot plotConfig "times-vs-optimal" $
                streamQuery variantQuery
                .| C.filter variantFilter
                .| C.mapM dataFromVariantInfo
                .| C.map (second $ translatePair . fmap V.convert)
