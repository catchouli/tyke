module Game.Rendering
  ( renderGame
  )
where

import Game.Data
import qualified Graphics.Gloss                  as Gloss
  

-- Render the game to a Gloss.Picture
renderGame :: Game -> Gloss.Picture
renderGame game =
  let count = _counter game
      colour = Gloss.Color Gloss.white
      playerTranslation = Gloss.translate 0 0
    in Gloss.Pictures [ colour . playerTranslation $ Gloss.circle 10
                      , colour $ Gloss.text (show count)
                      ]
