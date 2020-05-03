module Life
    ( start_game
    ) where
import           Data.Map
import           Data.Set
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Game

import           Constants
import           Controls
import           Graphics
import           Types
import           Utils

start_game :: IO ()
start_game = playIO FullScreen background_color default_fps init_state renderer handler updater

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

-- | Create empty Field
createField :: Field
createField = Data.Set.empty

-- | Create GameState with game at pause, speed 20 fps
init_state = GS createField True 3 0

-- | Count size of window
windowSize = both (* (round cellSize)) field_size

-- | Is it the time the field shall be refreshed
is_time_to_update :: Int -> Int -> Bool
is_time_to_update frame_counter allowed_frame = (frame_counter `mod` allowed_frame) == 0

-- | Get new current frame
update_current_frame :: Int -> Int
update_current_frame frame = (frame + 1) `mod` default_fps

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

