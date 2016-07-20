module Window
  ( gameInWindow
  )
where

import Framework

import Data.List (foldl')
import Linear (V2(..), V4(..))
import Control.Monad (unless)
import Data.Text (Text)
import Foreign.C.Types

import qualified SDL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss
import qualified Graphics.UI.GLUT.Initialization as GLUT

-- Run an application in a window using an initial state, and a given update and render function
gameInWindow :: Text -> (Int, Int) -> InputHandler -> TickHandler -> RenderHandler -> IO ()
gameInWindow title (width, height) inputHandler tickHandler renderHandler = do
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
  let renderPicture = Gloss.displayPicture (800, 600) Gloss.black glossState 1.0

  -- Main loop
  let loop = do -- Poll for events
                events <- SDL.pollEvents

                -- Check for a quit event
                let quit = any (== SDL.QuitEvent) . map SDL.eventPayload $ events

                -- Update the state and render
                mapM_ inputHandler events
                tickHandler 0.1
                print "render"
                renderHandler

                -- Swap buffer
                SDL.glSwapWindow window

                -- Loop, or quit if requested
                unless quit loop

    in loop

  -- Cleanup. Important for ghci use
  SDL.glDeleteContext context
  SDL.destroyWindow window
  SDL.quit
  GLUT.exit
