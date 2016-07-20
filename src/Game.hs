module Game
  ( gameNetwork
  , renderGame
  )
where

import Framework
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.Gloss                  as Gloss


data Game = Game { _counter :: Double
                 }


-- The overall game behavior, which takes input and tick events
-- and produces a Game
gameNetwork :: InputEvent -> TickEvent -> MomentIO (Behavior Game)
gameNetwork eInput eTick = do
  bCounter <- accumB 0 $ (+1) <$ eTick
  return $ Game <$> bCounter
  

-- Render the game to a Gloss.Picture
renderGame :: Game -> Gloss.Picture
renderGame game =
  let count = _counter game
      colour = Gloss.Color Gloss.white
      playerTranslation = Gloss.translate 0 0
    in Gloss.Pictures [ colour . playerTranslation $ Gloss.circle 10
                      , colour $ Gloss.text (show count)
                      ]
