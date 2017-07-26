import Control.Applicative (many)
import Data.Attoparsec.ByteString.Char8 (char, decimal, endOfLine)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as LBS
import System.Environment (getArgs)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

pointParser :: Parser [(Int, Int)]
pointParser = many $ (,) <$> decimal <* char ' ' <*> decimal <* endOfLine

main :: IO ()
main = do
    (name:_) <- getArgs
    input <- LBS.getContents
    case maybeResult (parse pointParser input) of
        Nothing -> putStrLn "Error!"
        Just ps -> toFile def (name ++ ".png") $ do
            layout_title .= name
            layout_plot_background .= Just (FillStyleSolid $ opaque white)
            layout_legend .= Nothing
            layout_top_axis_visibility . axis_show_line .= True
            layout_bottom_axis_visibility . axis_show_ticks .= False
            layout_bottom_axis_visibility . axis_show_labels .= False
            layout_right_axis_visibility . axis_show_line .= True
            layout_left_axis_visibility . axis_show_ticks .= False
            layout_left_axis_visibility . axis_show_labels .= False
            setColors [opaque blue]
            plot $ points "points" ps
