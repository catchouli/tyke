-- | 
-- Module      :  Window
-- Description :  Contains window and UI related functions
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Window
  ( gameInWindow
  )
where

import Framework

import Data.List (foldl')
import Linear (V2(..), V4(..))
import Control.Monad (unless, replicateM_)
import Data.Text (Text)
import Foreign.C.Types
import Data.Time.Clock.POSIX

import qualified SDL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss
import qualified Graphics.UI.GLUT.Initialization as GLUT



-- |  Runs a game at a fixed timestep, given an input, update, and render handler

gameInWindow :: Fractional a => Text
             -> TickType
             -> (Int, Int)
             -> InputHandler
             -> TickHandler
             -> RenderHandler
             -> IO ()
gameInWindow title fTimestep (width, height)
             inputHandler tickHandler renderHandler = do
  -- Initialise SDL
  SDL.initializeAll

  -- Construct window description
  let windowDims = V2 (fromIntegral width) (fromIntegral height)
  let windowDesc = SDL.defaultWindow { SDL.windowOpenGL = Just SDL.defaultOpenGL
                                     , SDL.windowInitialSize = windowDims
                                     }

  -- Create window
  window <- SDL.createWindow title windowDesc

  -- Create opengl context
  context <- SDL.glCreateContext window

  -- Initialise gloss
  glossState <- Gloss.initState

  -- Initailise glut. Used by gloss for text
  -- Otherwise using Gloss.text will cause a runtime crash but could otherwise
  -- be removed to remove the GLUT dependency
  GLUT.initialize "" []

  -- Function to render a gloss picture
  let renderPicture = Gloss.displayPicture (width, height) Gloss.black
                                           glossState 1.0

  let timestep = realToFrac fTimestep

  -- Main loop
  let loop lastTime = do 

        -- Current time
        currentTime <- getPOSIXTime

        -- Calculate passed timesteps and update lastTime
        let passedTime = currentTime - lastTime
        let passedTimeSteps = fromIntegral . round $ passedTime / realToFrac timestep
        let newLastTime = lastTime + passedTimeSteps * timestep

        -- Poll for events
        events <- SDL.pollEvents

        -- Check for a quit event
        let quit = any (== SDL.QuitEvent) . map SDL.eventPayload $ events

        -- Send events to the input event sink
        mapM_ inputHandler events

        -- Perform tick however many times we need to meet the fixed timestep
        replicateM_ 5 (tickHandler fTimestep)

        -- Render the game
        renderHandler

        -- Swap buffer
        SDL.glSwapWindow window

        -- Loop, or quit if requested
        unless quit (loop newLastTime)

    in getPOSIXTime >>= loop

  -- Cleanup. Important for ghci use
  SDL.glDeleteContext context
  SDL.destroyWindow window
  SDL.quit
  GLUT.exit
