-- | 
-- Module      :  Game.Rendering
-- Description :  Exposes the renderGame function and is the 
--                entry point into the main rendering path for the game
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Game.Rendering
  ( renderGame
  )
where

import Game.Data
import qualified Graphics.Gloss                  as Gloss
  

-- | Render the Game data to a gloss picture

renderGame :: Game -> Gloss.Picture
renderGame game =
  let count = _counter game
      colour = Gloss.Color Gloss.white
      playerTranslation = Gloss.translate 0 0
    in Gloss.Pictures [ colour . playerTranslation $ Gloss.circle 10
                      , colour $ Gloss.text (show count)
                      ]
