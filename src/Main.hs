{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game
import Window


width, height :: Int
(width, height) = (800, 600)

-- main
main :: IO ()
main = gameInWindow "Physy" (width, height) initialGameState update render
