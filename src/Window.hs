-- | 
-- Module      :  Window
-- Description :  Contains window and UI related functions
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Window
  ( gameLoop
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


-- |  Runs a game at a fixed timestep, given an input, update, and render handler

gameLoop :: Fractional a
             => SDL.Window
             -> TickType
             -> InputHandler
             -> TickHandler
             -> RenderHandler
             -> IO ()
gameLoop window fTimestep inputHandler tickHandler renderHandler = do

  let timestep = realToFrac fTimestep

  -- Main loop
  let loop lastTime = do 

        -- Current time
        currentTime <- getPOSIXTime

        -- Calculate passed timesteps and update lastTime
        let passedTime = currentTime - lastTime
        let passedTimeSteps = round $ passedTime / realToFrac timestep
        let newLastTime = lastTime + (fromIntegral passedTimeSteps) * timestep

        -- Poll for events
        events <- SDL.pollEvents

        -- Check for a quit event
        let quit = any (== SDL.QuitEvent) . map SDL.eventPayload $ events

        -- Send events to the input event sink
        mapM_ inputHandler events

        -- Perform tick however many times we need to meet the fixed timestep
        replicateM_ passedTimeSteps (tickHandler fTimestep)

        -- Render the game
        renderHandler

        -- Swap buffer
        SDL.glSwapWindow window

        -- Loop, or quit if requested
        unless quit (loop newLastTime)

    in getPOSIXTime >>= loop
