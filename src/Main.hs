{-# LANGUAGE OverloadedStrings #-}

module Main where

import Framework
import Game
import Window

import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss

width, height :: Int
(width, height) = (800, 600)

timestep :: Float
timestep = 1.0 / 60.0

-- main
main :: IO ()
main = do
  (input, tick, render) <- hostGame (width, height) gameNetwork renderGame
  gameInWindow "Physy" timestep (width, height) input tick render
