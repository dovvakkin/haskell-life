module Types where

import           Data.Set

type Cell = (Int, Int)
type Field = Set Cell

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

