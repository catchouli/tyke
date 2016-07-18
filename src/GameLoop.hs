module GameLoop
  ( gameLoop
  )
where

import qualified SDL.Event                       as SDL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss


-- The game loop
gameLoop :: IO ()
gameLoop = putStrLn "hi" >> gameLoop
