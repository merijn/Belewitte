module Main (main) where

import Options

import BarPlot (barPlot)
import PlotOptions (commands)

main :: IO ()
main = runSqlMCommand commands $ barPlot
