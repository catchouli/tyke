{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL (($=))
import Linear (V4(..))
import Control.Applicative
import Control.Monad (unless)

import qualified SDL
import qualified Graphics.Rendering.OpenGL       as GL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss
import qualified Graphics.UI.GLUT.Initialization as GLUT

(width, height) = (800, 600)

data Game = Game

main :: IO ()
main = do

  -- Initilaise SDL
  SDL.initializeAll
  window <- SDL.createWindow "Physy" SDL.defaultWindow { SDL.windowOpenGL = Just SDL.defaultOpenGL }
  SDL.glCreateContext window

  -- Initialise gloss
  glossState <- Gloss.initState

  -- Initailise glut. Used by gloss for text
  GLUT.initialize "" []

  let game = Game

  -- Main loop
  let loop = do events <- SDL.pollEvents
                let quit = any (== SDL.QuitEvent) . map SDL.eventPayload $ events
                render window glossState game
                SDL.glSwapWindow window
                unless quit loop
    in loop

  -- Cleanup. Important for ghci use
  SDL.destroyWindow window
  SDL.quit
  GLUT.exit

render :: SDL.Window -> Gloss.State -> Game -> IO ()
render window glossState game =
  Gloss.displayPicture (width, height) Gloss.black glossState 1.0 $
                       Gloss.Pictures [ Gloss.Color Gloss.white $ Gloss.circle 10
                                      , Gloss.Color Gloss.white $ Gloss.text "hello"
                                      ]
