{-# LANGUAGE LambdaCase #-}

module Game
  ( initialGameState
  , update
  , render
  )
where

import qualified SDL.Event                       as SDL
import qualified SDL.Input.Keyboard              as SDL
import qualified SDL.Input.Keyboard.Codes        as SDL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss


data Game = Game { counter :: Int
                 , pos :: (Int, Int)
                 }


-- Function to render a gloss picture
initialGameState = Game 0 (0, 0)


-- Update the game
update :: Game -> Game
update = id


-- Render the game to a Gloss.Picture
render :: Game -> Gloss.Picture
render game =
  let count = counter game
      (playerX, playerY) = pos game
    in Gloss.Pictures [ Gloss.Color Gloss.white $ Gloss.translate (fromIntegral playerX) (fromIntegral playerY) $ Gloss.circle 10
                      , Gloss.Color Gloss.white $ Gloss.text (show count)
                      ]
