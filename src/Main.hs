{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game (gameNetwork, renderGame)
import Framework (hostGame)
import Window (gameLoop)
import Linear.V2
import qualified SDL
import ImGui


-- | The window title

title = "tyke"


-- | The width and height of the game window

width, height :: Int
(width, height) = (800, 600)


-- | The fixed timestep of the game
-- Every iteration of the main loop, the game will tick as many
-- times as required in order to mainain a frequency of 1 / timestep

timestep :: Float
timestep = 1.0 / 60.0


-- | The entry point of the application

main :: IO ()
main = do
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
  SDL.swapInterval SDL.$= SDL.ImmediateUpdates

  -- Initialise imgui
  initialiseGui

  -- Generate render function (it has some state in IO)
  renderFun <- renderGame

  -- Set up game event sinks (input, tick and render)
  (input, tick, render) <- hostGame (width, height) gameNetwork renderFun

  -- Run the game
  gameLoop window timestep input tick render

  -- Cleanup. Important for ghci use
  SDL.glDeleteContext context
  SDL.destroyWindow window
  SDL.quit
