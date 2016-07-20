{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game (gameNetwork, renderGame)
import Framework (hostGame)
import Window (gameInWindow)


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
  (input, tick, render) <- hostGame (width, height) gameNetwork renderGame
  gameInWindow "Physy" timestep (width, height) input tick render
