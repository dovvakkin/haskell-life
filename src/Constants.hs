module Constants where

import           Data.Set
import           Graphics.Gloss

field_size@(field_width, field_height) = (40, 25) :: (Int, Int)

cellSize = 24 :: Float

default_fps = 60 :: Int

n_neighbours_to_born = Data.Set.fromList [3 :: Int]
n_neighbours_to_stay = Data.Set.fromList [2 :: Int]

background_color = makeColorI 41 44 51 0

