{-# LANGUAGE OverloadedStrings #-}

module Main where

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
main = gameInWindow "Physy" (width, height) initialGameState update render
