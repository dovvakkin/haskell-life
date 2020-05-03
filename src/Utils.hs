module Utils where

import           Types

-- | Convert list of numbers to list of pairs (x, y)
list_to_tup :: [Int] -> [Cell]
list_to_tup (a:b:xs) = (a,b) : list_to_tup xs
list_to_tup _        = []

-- | Some sort of map() for tuple
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

