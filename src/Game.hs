module Game
  ( gameNetwork
  , renderGame
  )
where

import Framework
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.Gloss                  as Gloss


data Game = Game { _counter :: Float
                 }


-- The overall game behavior, which takes input and tick events
-- and produces a Game
gameNetwork :: InputEvent -> TickEvent -> MomentIO (Behavior Game)
gameNetwork eInput eTick = do
  -- Current time as an event
  eTime <- accumE 0 ((+) <$> eTick)

  -- Current time as a behavior
  bTime <- stepper 0 eTime

  -- The overall game behavior
  return $ Game <$> bTime
  

-- Render the game to a Gloss.Picture
renderGame :: Game -> Gloss.Picture
renderGame game =
  let count = _counter game
      colour = Gloss.Color Gloss.white
      playerTranslation = Gloss.translate 0 0
    in Gloss.Pictures [ colour . playerTranslation $ Gloss.circle 10
                      , colour $ Gloss.text (show count)
                      ]
