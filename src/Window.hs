module Window
  ( gameInWindow
  )
where

import Linear (V2(..), V4(..))
import Control.Monad (unless)
import Data.Text (Text)
import Foreign.C.Types

import qualified SDL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss
import qualified Graphics.UI.GLUT.Initialization as GLUT


-- Run an application in a window using a given update, render, and input function
gameInWindow :: Text -> (Int, Int) -> a -> (a -> a) -> (a -> Gloss.Picture) -> IO ()
gameInWindow title (width, height) initialState update render = do
  -- Initialise SDL
  SDL.initializeAll
  window <- SDL.createWindow title SDL.defaultWindow { SDL.windowOpenGL = Just SDL.defaultOpenGL
                                                     , SDL.windowInitialSize = V2 (fromIntegral width) (fromIntegral height)
                                                     }

  -- Create opengl context
  context <- SDL.glCreateContext window

  -- Initialise gloss
  glossState <- Gloss.initState

  -- Initailise glut. Used by gloss for text
  -- Otherwise using Gloss.text will cause a runtime crash but could otherwise
  -- be removed to remove the GLUT dependency
  GLUT.initialize "" []

  -- Function to render a gloss picture
  let renderGame = Gloss.displayPicture (width, height) Gloss.black glossState 1.0

  -- Main loop
  let loop state = do events <- SDL.pollEvents
                      let quit = any (== SDL.QuitEvent) . map SDL.eventPayload $ events
                      let newState = update state
                      renderGame $ render newState
                      SDL.glSwapWindow window
                      unless quit (loop newState)
    in loop initialState

  -- Cleanup. Important for ghci use
  SDL.glDeleteContext context
  SDL.destroyWindow window
  SDL.quit
  GLUT.exit
