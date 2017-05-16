-- | 
-- Module      :  Window
-- Description :  Contains window and UI related functions
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

{-# LANGUAGE LambdaCase #-}

module Window
  ( gameLoop
  )
where

import Framework

import Data.List (foldl')
import Linear (V2(..), V4(..))
import Control.Monad (unless, replicateM_, void)
import Data.Text (Text)
import Foreign.C.Types
import Data.Time.Clock.POSIX
import Control.Concurrent.MVar
import Control.Monad (forM_)
import ImGui

import qualified SDL


-- |  Runs a game at a fixed timestep, given an input, update, and render handler

gameLoop :: SDL.Window
         -> TickType
         -> InputHandler
         -> TickHandler
         -> RenderHandler
         -> IO ()
gameLoop window fTimestep inputHandler tickHandler renderHandler = do

  let timestep = realToFrac fTimestep

  quit <- newMVar False
  let exit = void $ swapMVar quit True

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
        forM_ (map SDL.eventPayload events)
          (\case
             SDL.QuitEvent       -> exit
             SDL.KeyboardEvent d -> case (SDL.keysymScancode . SDL.keyboardEventKeysym) d of
                                         SDL.ScancodeEscape -> exit
                                         _                  -> return ()
             _                   -> return ()
          )
          
        -- Get input data
        mb <- SDL.getMouseButtons
        mousePos <- SDL.getAbsoluteMouseLocation >>= \(SDL.P (SDL.V2 x y)) -> return (fromIntegral x, fromIntegral y)
        let mouseDown = ( mb SDL.ButtonLeft, mb SDL.ButtonMiddle
                        , mb SDL.ButtonRight, mb SDL.ButtonX1, mb SDL.ButtonX2
                        )
        let mouseWheel = 0
        let keyCtrl = False
        let keyShift = False
        let keyAlt = False
        let keySuper = False
        let inputData = InputData mousePos mouseDown mouseWheel keyCtrl keyShift keyAlt keySuper
                    
        -- Send events to the input event sink
        mapM_ inputHandler events

        -- Perform tick however many times we need to meet the fixed timestep
        replicateM_ passedTimeSteps (tickHandler fTimestep)

        -- Render the game
        renderHandler

        newFrame inputData
        renderGui

        -- Swap buffer
        SDL.glSwapWindow window

        -- Loop, or quit if requested
        quitting <- readMVar quit
        unless quitting (loop newLastTime)

    in getPOSIXTime >>= loop
