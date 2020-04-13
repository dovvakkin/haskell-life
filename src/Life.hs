module Life
    ( start_game
    ) where

import           Data.Map
import           Data.Set
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.Pure.Game

field_size@(field_width, field_height) = (20, 20) :: (Int, Int)

cellSize = 24 :: Float

n_neighbours_to_born = Data.Set.fromList [3]
n_neighbours_to_stay = Data.Set.fromList [2]

type Cell = (Int, Int)
type Field = Set Cell

start_game :: IO ()
start_game = play (InWindow "Hsweeper" windowSize (240, 160)) white 7 init_state renderer handler updater

get_coordinate_on_torus :: Cell -> Cell
get_coordinate_on_torus (cx, cy) = ((cx + field_width) `mod` field_width,
                                   (cy + field_height) `mod` field_height)

-- | Return coordinates on torus of all neighbours of given Cell
get_neighbours :: Cell -> [Cell]
get_neighbours c@(cx, cy) = Prelude.map get_coordinate_on_torus $
                          [ (i, j) | i <- [cx - 1 .. cx + 1], j <- [cy - 1 .. cy + 1],
                          (i, j) /= c
                          ]

-- | Return set of neighbours for all Cells from Field
get_all_neighbours :: Field -> [Cell]
get_all_neighbours f = Data.Set.foldl (\a b -> a ++ get_neighbours b ) [] f

-- | Return Cells and number it's occurences in [Cells]
list_to_count :: [Cell] -> [(Cell, Int)]
list_to_count ls = Data.Map.toList (Data.Map.fromListWith (+) [(l, 1) | l <- ls])

-- | Return
field_to_counter :: Field -> [(Cell, Int)]
field_to_counter f = list_to_count $get_all_neighbours f

-- | Return True if in Cell with Int neighbours should born life else False
is_born_with_neighbours :: (Cell, Int) -> Bool
is_born_with_neighbours (_, n) = n `Data.Set.member` n_neighbours_to_born

-- | Return True if in Cell with Int and life life should stay
is_stay_with_neighbours :: (Cell, Int) -> Bool
is_stay_with_neighbours (_, n) = n `Data.Set.member` n_neighbours_to_stay

-- | Get list of Cells and it's counts as neighbours,
--   Return next Field state
counter_to_field :: [(Cell, Int)] -> Field -> Field
counter_to_field l f = Data.Set.fromList
                     $ Prelude.map fst
		     $ Prelude.filter (\x -> (is_stay_with_neighbours x) && ((fst x) `Data.Set.member` f) ||
	                                     is_born_with_neighbours x) l

-- | Get Field state,
--   Return next Field state
get_new_field :: Field -> Field
get_new_field f = counter_to_field (field_to_counter f) f

data GameState = GS
    { field :: Field
    , pause :: Bool
    }

createField :: Field
createField = Data.Set.empty

init_state = GS createField True

-- | If life in Cell, return Field where no life in Cell,
--   if no life in Cell, return Field where life in Cell
modify_life :: Cell -> Field -> Field
modify_life c f
	| c `Data.Set.member` f = Data.Set.delete c f
 	| otherwise = Data.Set.insert c f

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

windowSize = both (* (round cellSize)) field_size

updater time gs@GS
    { field = field
    , pause = False
    } = gs
    { field = new_field
    , pause = False
    } where
    new_field = get_new_field field

updater _ gs = gs

handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { field = field
    , pause = True
    } = gs
    { field = newField
    , pause = True
    } where
    newField = modify_life (screenToCell mouse) field

handler (EventKey (Char 'p') Down _ _) gs@GS
    { field = field
    , pause = pause
    } = gs
    { field = field
    , pause = not pause
    }

handler _ gs = gs

screenToCell = both (round . (/ cellSize)) . invertViewPort viewPort
cellToScreen = both ((* cellSize) . fromIntegral)

renderer GS { field = field} = applyViewPortToPicture viewPort $ pictures $ cells ++ grid where
    grid = [uncurry translate (cellToScreen (x, y)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. field_width - 1], y <- [0 .. field_height - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) $ draw_cell x y | x <- [0 .. field_width - 1], y <- [0 .. field_height - 1]]
    draw_cell x y
        | (x, y) `Data.Set.member` field = color green $ rectangleSolid cellSize cellSize
	| otherwise = color white $ rectangleSolid cellSize cellSize

viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen field_size) 0 1


