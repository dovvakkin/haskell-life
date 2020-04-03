import Data.Map
import Data.Set

field_width = 20 :: Int
field_height = 20 :: Int

n_neighbours_to_live = Data.Set.fromList [2, 3] 

type Cell = (Int, Int)
type Field = Set Cell

get_coordinate_on_torus :: Cell -> Cell
get_coordinate_on_torus (cx, cy) = ((cx + field_width) `mod` field_width,
                                   (cy + field_height) `mod` field_height)

get_neighbours :: Cell -> [Cell]
get_neighbours c@(cx, cy) = Prelude.map get_coordinate_on_torus $
                          [ (i, j) | i <- [cx - 1 .. cx + 1], j <- [cy - 1 .. cy + 1],
                          (i, j) /= c
                          ]

get_all_neighbours :: Field -> [Cell]
get_all_neighbours f = Data.Set.foldl (\a b -> a ++ get_neighbours b ) [] f

list_to_count :: [Cell] -> [(Cell, Int)]
list_to_count ls = Data.Map.toList (Data.Map.fromListWith (+) [(l, 1) | l <- ls])

field_to_counter :: Field -> [(Cell, Int)]
field_to_counter f = list_to_count $ get_all_neighbours f

is_life_with_neighbours :: (Cell, Int) -> Bool
is_life_with_neighbours (_, n) = n `Data.Set.member` n_neighbours_to_live

counter_to_field :: [(Cell, Int)] -> Field
counter_to_field l = Data.Set.fromList
                     $ Prelude.map fst
		     $ Prelude.filter is_life_with_neighbours l

get_new_field :: Field -> Field
get_new_field f = counter_to_field $ field_to_counter f

