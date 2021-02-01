{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Options

import BarPlot (barPlot)
import Heatmap (plotHeatmap)
import Interesting (findInterestingVariants)
import PlotOptions (PlotCommand(..), commands)

main :: IO ()
main = runSqlMCommand commands $ \case
    PlotBar config -> barPlot config
    PlotHeatmap config -> plotHeatmap config
    ReportInteresting vFilter vInfoCfg implFilters summary ->
        findInterestingVariants vFilter vInfoCfg implFilters summary
