{-# LANGUAGE OverloadedStrings #-}

module Main where

import Framework
import Game
import GameLoop
import Window

import Control.Wire

import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss

width, height :: Int
(width, height) = (800, 600)

-- main
main :: IO ()
main = do
  (input, tick, render) <- hostGame (width, height) gameNetwork renderGame
  gameInWindow "Physy" (width, height) input tick render
