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


-- The overall game behavior, which takes input and tick events and produces a Game
gameNetwork :: InputEvent -> TickEvent -> MomentIO (Behavior Game)
gameNetwork eInput eTick = do
  bCounter <- accumB 0 $ (+1) <$ eTick
  return $ Game <$> bCounter
  

-- Render the game to a Gloss.Picture
renderGame :: Game -> Gloss.Picture
renderGame game =
  let count = _counter game
      (playerX, playerY) = (0, 0)
    in Gloss.Pictures [ Gloss.Color Gloss.white $ Gloss.translate (fromIntegral playerX) (fromIntegral playerY) $ Gloss.circle 10
                      , Gloss.Color Gloss.white $ Gloss.text (show count)
                      ]
