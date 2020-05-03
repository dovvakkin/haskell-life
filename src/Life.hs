module Life
    ( start_game
    ) where


import           Data.Map
import           Data.Set
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Game
--import           Graphics.Gloss.Interface.Pure.Game
import           System.Directory

import           System.IO

field_size@(field_width, field_height) = (40, 25) :: (Int, Int)

cellSize = 24 :: Float

default_fps = 60 :: Int

n_neighbours_to_born = Data.Set.fromList [3]
n_neighbours_to_stay = Data.Set.fromList [2]

type Cell = (Int, Int)
type Field = Set Cell

start_game :: IO ()
start_game = playIO FullScreen white default_fps init_state renderer handler updater

-- | Return coordinate on field that has the shape of the torus
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

-- | Return the number of neighbours for all Cells
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


-- | field is Set of Cells, Cells are tuples (x, y)
--   pause is boolean derivative that represents the current game mode -- pause or simulation
--   allowed_frame and current_frame are responsible for game speed
--   allowed_frame shows every frame we accept, so if current_frame is divided by allowed_frame then we allow it
data GameState = GS
    { field         :: Field
    , pause         :: Bool
    , allowed_frame :: Int
    , current_frame :: Int
    }

-- | Create empty Field
createField :: Field
createField = Data.Set.empty

-- | Create GameState with game at pause, speed 20 fps
init_state = GS createField True 3 0

-- | If life in Cell, return Field where no life in Cell,
--   if no life in Cell, return Field where life in Cell
modify_life :: Cell -> Field -> Field
modify_life c f
	| c `Data.Set.member` f = Data.Set.delete c f
 	| otherwise = Data.Set.insert c f

-- | Some sort of map() for tuple
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

-- | Count size of window
windowSize = both (* (round cellSize)) field_size

-- | Is it the time the field shall be refreshed
is_time_to_update :: Int -> Int -> Bool
--is_time_to_update _ _ = True
--is_time_to_update time allowed_frame = round ((get_fractional time) / (1 / default_fps)) == 1
is_time_to_update frame_counter allowed_frame = (frame_counter `mod` allowed_frame) == 0

-- | Get new current frame
update_current_frame :: Int -> Int
update_current_frame frame = (frame + 1) `mod` default_fps

-- | Increase speed with decreasing the allowed frame
increase_speed :: Int -> Int
increase_speed allowed_frame | allowed_frame > 1 = allowed_frame - 1
                          | otherwise = allowed_frame

-- | Decrease speed with decreasing the allowed frame
decrease_speed :: Int -> Int
decrease_speed allowed_frame | allowed_frame < default_fps = allowed_frame + 1
                          | otherwise = allowed_frame


-- | Update the current field according the Life rules (only in simulation mode)
--   Don't update if the current frame is not allowed (not diveded by allowed_frame)
updater :: Float -> GameState -> IO GameState
updater time gs@GS
    { field = field
    , pause = False
    , allowed_frame = allowed_frame
    , current_frame = current_frame
    }
  | is_time_to_update current_frame allowed_frame = return gs
    { field = get_new_field field
    , pause = False
    , allowed_frame = allowed_frame
    , current_frame = update_current_frame current_frame
    }
  | otherwise = return gs
    { field = field
    , pause = False
    , allowed_frame = allowed_frame
    , current_frame = update_current_frame current_frame
    }

updater _ gs = return gs
-- | Thus updater changes the GameState on time


-- | handler changes the GameState on event
--   handler may change the game speed (increase/decrease) and mode (pause/simulation) at any time
--   in pause mode handler works with Cells on field according to mouse getsures
--   also handler can save the current fiels to file "saved_state.hls"
handler :: Event -> GameState -> IO GameState
handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { field = field
    , pause = True
    } = return gs
    { field = newField
    , pause = True
    } where
    newField = modify_life (screenToCell mouse) field  -- this adds/removes life from cell in pause mode

handler (EventKey (Char 'p') Down _ _) gs@GS
    { field = field
    , pause = pause
    } = return gs
    { field = field
    , pause = not pause
    }  -- this changes the game mode

handler (EventKey (Char ',') Down _ _) gs@GS
    { field = field
    , pause = pause
    , allowed_frame = allowed_frame
    } = return gs
    { field = field
    , pause = pause
    , allowed_frame = decrease_speed allowed_frame
    }  -- this decreases the game speed

handler (EventKey (Char '.') Down _ _) gs@GS
    { field = field
    , pause = pause
    , allowed_frame = allowed_frame
    } = return gs
    { field = field
    , pause = pause
    , allowed_frame = increase_speed allowed_frame
    }  -- this increases the game speed

handler (EventKey (Char 's') Down _ _) gs@GS
    { field = field
    , pause = True
    } = do
	outh <- openFile "saved_state.hls" WriteMode
	mapM (\x -> do
                      hPrint outh (fst x)
		      hPrint outh (snd x)) (Data.Set.toList field)
        hClose outh
	return gs  -- this saves field to file

handler (EventKey (Char 'l') Down _ _) gs@GS
    { field = field
    , pause = True
    } = do
	saveExists <- doesFileExist "saved_state.hls"
        if saveExists
	    then do
            content <- readFile "saved_state.hls"
	    return gs { field = Data.Set.fromList (list_to_tup (Prelude.map (read::String->Int) (lines content)))
  	    , pause = True}
        else
	    return gs  -- this loads fiels from file if it exists

handler _ gs = return gs


-- | Convert list of numbers to list of pairs (x, y)
list_to_tup :: [Int] -> [Cell]
list_to_tup (a:b:xs) = (a,b) : list_to_tup xs
list_to_tup _        = []

-- | Get Cell from mouse coordinates
screenToCell = both (round . (/ cellSize)) . invertViewPort viewPort
cellToScreen = both ((* cellSize) . fromIntegral)

-- | Draw current GameState
renderer :: GameState -> IO Picture
renderer GS { field = field} = return (applyViewPortToPicture viewPort $ pictures $ cells ++ grid) where
    grid = [uncurry translate (cellToScreen (x, y)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. field_width - 1], y <- [0 .. field_height - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) $ draw_cell x y | x <- [0 .. field_width - 1], y <- [0 .. field_height - 1]]
    draw_cell x y
        | (x, y) `Data.Set.member` field = color green $ rectangleSolid cellSize cellSize
	| otherwise = color white $ rectangleSolid cellSize cellSize


add_guide_space :: (Int, Int) -> (Int, Int)
add_guide_space (x, y) = (x + 12, y)
-- | For picture rendering
viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen (add_guide_space field_size)) 0 1


