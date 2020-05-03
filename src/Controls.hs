module Controls where

import           Data.Set
import           Graphics.Gloss.Interface.IO.Game
import           System.Directory
import           System.Exit
import           System.IO

import           Constants
import           Graphics
import           Types
import           Utils

modify_life :: Cell -> Field -> Field
modify_life c f
	| c `Data.Set.member` f = Data.Set.delete c f
 	| otherwise = Data.Set.insert c f

-- | Increase speed with decreasing the allowed frame
increase_speed :: Int -> Int
increase_speed allowed_frame | allowed_frame > 1 = allowed_frame - 1
                          | otherwise = allowed_frame

-- | Decrease speed with decreasing the allowed frame
decrease_speed :: Int -> Int
decrease_speed allowed_frame | allowed_frame < default_fps = allowed_frame + 1
                          | otherwise = allowed_frame

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

handler (EventKey (Char 'q') Down _ _) gs = do
    exitSuccess

handler _ gs = return gs

