module Game
  ( initialGameState
  , render
  )
where

import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss


data Game = Game

-- The initial game state
initialGameState :: Game
initialGameState = Game


-- Render the game to a Gloss.Picture
render :: Game -> Gloss.Picture
render game =
   Gloss.Pictures [ Gloss.Color Gloss.white $ Gloss.circle 10
                  , Gloss.Color Gloss.white $ Gloss.text "hello"
                  ]
