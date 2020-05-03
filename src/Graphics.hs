module Graphics where

import           Data.Set
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Game

import           Constants
import           Types
import           Utils

-- | Get Cell from mouse coordinates
screenToCell :: Point -> (Int, Int)
screenToCell = (both (round . (/ cellSize)) . invertViewPort viewPort)
cellToScreen = both ((* cellSize) . fromIntegral)

-- | Draw current GameState
renderer :: GameState -> IO Picture
renderer GS { field = field} = return (applyViewPortToPicture viewPort $ pictures $ cells ++ grid) where
    grid = [uncurry translate (cellToScreen (x, y)) $ color white $ rectangleWire cellSize cellSize | x <- [0 .. field_width - 1], y <- [0 .. field_height - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) $ draw_cell x y | x <- [0 .. field_width - 1], y <- [0 .. field_height - 1]]
    draw_cell x y
        | (x, y) `Data.Set.member` field = color green $ rectangleSolid cellSize cellSize
	| otherwise = color background_color $ rectangleSolid cellSize cellSize


add_guide_space :: (Int, Int) -> (Int, Int)
add_guide_space (x, y) = (x + 12, y)
-- | For picture rendering
viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen (add_guide_space field_size)) 0 1

