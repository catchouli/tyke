{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game
import GameLoop
import Window


width, height :: Int
(width, height) = (800, 600)

-- main
main :: IO ()
main = do (input, update, render) <- gameLoop gameNetwork
          gameInWindow "Physy" (width, height) input update render
