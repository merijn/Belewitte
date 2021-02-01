{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PlotOptions (PlotCommand(..), commands) where

import Control.Monad (forM)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C
import Data.Foldable (asum)
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S

import Core
import Schema
import Sql ((==.))
import qualified Sql
import Options

import BarPlot
import GlobalPlotOptions
import Heatmap
import Interesting
import Query.Dump (plotQueryDump)
import Query.Level (levelTimePlotQuery)
import Query.Time (timePlotQuery)
import Query.Variant (VariantInfoConfig(..))

data PlotCommand
    = PlotBar BarPlot
    | PlotHeatmap Heatmap
    | ReportInteresting
        { variantFilter :: VariantFilter
        , variantConfigInfo :: VariantInfoConfig
        , implFilter :: ImplFilter
        , shortSummary :: Bool
        }

queryVariants :: Key Algorithm -> IntervalSet Int64 -> SqlM (Set (Key Variant))
queryVariants algoId graphs = do
    gids <- Sql.selectKeys [] [] $
        C.filter (\i -> IS.member (fromSqlKey i) graphs)
        .| C.foldMap S.singleton

    variantConfigId <- Sql.selectKeysList
        [VariantConfigAlgorithmId ==. algoId, VariantConfigIsDefault ==. Active]
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

barPlotParser :: Parser (BarPlotType -> SqlM BarPlot)
barPlotParser = do
    getGlobalOpts <- globalOptionsParser
    slideFormat <- slideFlag
    printStdout <- printFlag
    rotateLabels <- rotateFlag
    graphSet <- intervalFlag "graphs" "graph"

    pure $ \barPlotType -> do
        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts

        BarPlot globalOpts barPlotType slideFormat printStdout rotateLabels
            <$> queryVariants globalPlotAlgorithm graphSet
  where
    slideFlag :: Parser Bool
    slideFlag = flag False True $ mconcat
        [ long "slide", help "Render 4:3 slide dimensions" ]

    printFlag :: Parser Bool
    printFlag = flag False True $ mconcat
        [ long "print", help "Print results to stdout, rather than plotting" ]

    rotateFlag :: Parser Bool
    rotateFlag = flag False True $ mconcat
        [ long "rotate", help "Rotate X axis labels" ]

variantSelectionOption :: Parser (Key Algorithm -> SqlM VariantSelection)
variantSelectionOption = asum
    [ fmap (fmap ConfigSelection) <$> variantConfigIdParser
    , pure $ const (return Everything)
    ]

totalsHeatmapParser :: Parser (SqlM Heatmap)
totalsHeatmapParser = do
    getGlobalOpts <- globalOptionsParser
    getVariantSelection <- variantSelectionOption
    showOptimal <- showOptimalFlag
    getDatasetId <- optional datasetIdParser

    pure $ do
        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts
        TotalHeatmap globalOpts
            <$> getVariantSelection globalPlotAlgorithm
            <*> sequence getDatasetId <*> pure showOptimal
  where
    showOptimalFlag :: Parser Bool
    showOptimalFlag = flag False True $ mconcat [long "show-optimal"]

levelsHeatmapParser :: Parser (SqlM Heatmap)
levelsHeatmapParser = do
    getGlobalOpts <- globalOptionsParser
    getVariantId <- variantIdParser

    pure $ do
        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts
        LevelHeatmap globalOpts <$> getVariantId globalPlotAlgorithm

predictHeatmapParser :: Parser (SqlM Heatmap)
predictHeatmapParser = do
    getGlobalOpts <- globalOptionsParser
    getVariantSelection <- variantSelectionOption
    getDatasetId <- optional datasetIdParser
    getPredictorConfigs <- predictorConfigsParser

    pure $ do
        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts

        PredictHeatmap globalOpts
            <$> getVariantSelection globalPlotAlgorithm
            <*> sequence getDatasetId <*> getPredictorConfigs

commands :: CommandRoot (SqlM PlotCommand)
commands = CommandRoot
  { mainHeaderDesc = "a tool for plotting benchmark results"
  , mainDesc = ""
  , mainQueryDump = plotQueryDump
  , mainQueryMap = plotQueryMap
  , mainCommands = SubCommands
    [ fmap PlotBar <$> CommandGroup CommandInfo
        { commandName = "bar"
        , commandHeaderDesc = "bar plots"
        , commandDesc = ""
        }
        [ SingleCommand CommandInfo
            { commandName = "levels"
            , commandHeaderDesc = "plot level times for a graph"
            , commandDesc = ""
            }
            $ barPlotParser <*> pure Levels
        , SingleCommand CommandInfo
            { commandName = "totals"
            , commandHeaderDesc = "plot total times for a set of graphs"
            , commandDesc = ""
            }
            $ barPlotParser <*> (Totals <$> normaliseFlag <*> useGraphIdFlag)
        , SingleCommand CommandInfo
            { commandName = "vs-optimal"
            , commandHeaderDesc =
            "plot total times for a set of graphs against the optimal"
            , commandDesc = ""
            }
            $ barPlotParser <*> (VsOptimal <$> normaliseFlag)
        ]
    , fmap PlotHeatmap <$> CommandGroup CommandInfo
        { commandName = "heatmap"
        , commandHeaderDesc = "heatmap plots"
        , commandDesc = ""
        }
        [ SingleCommand CommandInfo
            { commandName = "total"
            , commandHeaderDesc = "plot heatmap for all variants"
            , commandDesc = ""
            }
            $ totalsHeatmapParser
        , SingleCommand CommandInfo
            { commandName = "levels"
            , commandHeaderDesc = "plot heatmap for a variant"
            , commandDesc = ""
            }
            $ levelsHeatmapParser
        , SingleCommand CommandInfo
            { commandName = "predict"
            , commandHeaderDesc = "plot heatmap for a predictor"
            , commandDesc = ""
            }
            $ predictHeatmapParser
        ]
    , SingleCommand CommandInfo
        { commandName = "report"
        , commandHeaderDesc = "report interesting variants"
        , commandDesc = "Highlights variants of interest for various criteria"
        }
        $ reportParser
    ]
  }
  where
    normaliseFlag :: Parser Bool
    normaliseFlag = flag False True $ mconcat
        [ long "normalise"
        , help "Normalise bars to slowest implementation"
        ]

    useGraphIdFlag :: Parser Bool
    useGraphIdFlag = flag False True $ mconcat
        [ long "use-graph-ids"
        , help "Label groups with the graph's id, rather than name"
        ]

    summaryFlag :: Parser Bool
    summaryFlag = switch $ mconcat
        [ long "summary"
        , help "Print only a short summary of the interesting variants."
        ]

    reportParser :: Parser (SqlM PlotCommand)
    reportParser = do
        getVariantInfoConfig <- variantInfoConfigParser
        getVariantConfigId <- optional variantConfigIdParser
        minEdges <- optional $ minPropParser "edge" "edges"
        minVertices <- optional $ minPropParser "vertex" "vertices"
        filterFun <- intMapFilter "impl-set" "implementation"
        summary <- summaryFlag

        pure $ do
            cfg <- getVariantInfoConfig
            variantConfigId <- sequence $
                getVariantConfigId <*> pure (variantInfoAlgorithm cfg)

            let vFilter = VFilter
                    { filterVariantConfigId = variantConfigId
                    , filterEdgeSize = minEdges
                    , filterVertexSize = minVertices
                    }

            return $ ReportInteresting vFilter cfg filterFun summary
      where
        minPropParser :: String -> String -> Parser Int
        minPropParser name desc = option auto $ mconcat
            [ long $ "min-" <> name <> "-count"
            , help $ "Minimum number of " <> desc <> " required for a graph \
                     \to be considered."
            ]

plotQueryMap :: Map String (Parser DebugQuery)
plotQueryMap = M.fromList
    [ nameDebugQuery "timePlotQuery" . Compose $ do
        getAlgorithmId <- algorithmIdParser
        getPlatformId <- platformIdParser
        getCommit <- commitIdParser
        getVariants <- variantsParser

        pure $ do
            algoId <- getAlgorithmId
            timePlotQuery algoId
                <$> getPlatformId <*> getCommit algoId <*> getVariants algoId
    , nameDebugQuery "levelTimePlotQuery" . Compose $ do
        getAlgorithmId <- algorithmIdParser
        getPlatformId <- platformIdParser
        getCommit <- commitIdParser
        getVariantId <- variantIdParser

        pure $ do
            algoId <- getAlgorithmId
            levelTimePlotQuery
                <$> getPlatformId <*> getCommit algoId <*> getVariantId algoId
    ]
  where
    variantsParser :: Parser (Key Algorithm -> SqlM (Set (Key Variant)))
    variantsParser = do
        variants <- some variantIdParser
        pure $ \algoId -> S.fromList <$> traverse ($algoId) variants
