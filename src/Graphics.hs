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
renderer GS { field = field} = return (applyViewPortToPicture viewPort $ pictures $ cells ++ grid ++ pause_text ++ quit_text ++ save_text ++ load_text ++ up_text ++ down_text ++ load_pause_1_text ++ load_pause_2_text) where
    grid = [uncurry translate (cellToScreen (x, y)) $ color white $ rectangleWire cellSize cellSize | x <- [0 .. field_width - 1], y <- [0 .. field_height - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) $ draw_cell x y | x <- [0 .. field_width - 1], y <- [0 .. field_height - 1]]
    draw_cell x y
        | (x, y) `Data.Set.member` field = color green $ rectangleSolid cellSize cellSize
	| otherwise = color background_color $ rectangleSolid cellSize cellSize

pause_text = [ translate (fromIntegral ((fst field_size) * (round cellSize))) (fromIntegral (((snd field_size) - 5) * (round cellSize))) $ Scale 0.35 0.35 $ Color white $ Text "p pause"]

quit_text = [ translate (fromIntegral ((fst field_size) * (round cellSize))) (fromIntegral (((snd field_size) - 7) * (round cellSize))) $ Scale 0.35 0.35 $ Color white $ Text "q quit"]

save_text = [ translate (fromIntegral ((fst field_size) * (round cellSize))) (fromIntegral (((snd field_size) - 9) * (round cellSize))) $ Scale 0.35 0.35 $ Color white $ Text "s save"]

load_text = [ translate (fromIntegral ((fst field_size) * (round cellSize))) (fromIntegral (((snd field_size) - 11) * (round cellSize))) $ Scale 0.35 0.35 $ Color white $ Text "l load"]

up_text = [ translate (fromIntegral ((fst field_size) * (round cellSize))) (fromIntegral (((snd field_size) - 13) * (round cellSize))) $ Scale 0.35 0.35 $ Color white $ Text "> speed up"]

down_text = [ translate (fromIntegral ((fst field_size) * (round cellSize))) (fromIntegral (((snd field_size) - 15) * (round cellSize))) $ Scale 0.35 0.35 $ Color white $ Text "< speed down"]

load_pause_1_text = [ translate (fromIntegral ((fst field_size) * (round cellSize))) (fromIntegral (((snd field_size) - 19) * (round cellSize))) $ Scale 0.35 0.35 $ Color white $ Text "save, load"]

load_pause_2_text = [ translate (fromIntegral ((fst field_size) * (round cellSize))) (fromIntegral (((snd field_size) - 21) * (round cellSize))) $ Scale 0.35 0.35 $ Color white $ Text "only on pause"]

add_guide_space :: (Int, Int) -> (Int, Int)
add_guide_space (x, y) = (x + 12, y)
-- | For picture rendering
viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen (add_guide_space field_size)) 0 1

