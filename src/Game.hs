module Game
  ( gameNetwork
  )
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified SDL.Event                       as SDL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss


data Game = Game { counter :: Int
                 }

-- Function to render a gloss picture
renderGame glossState = Gloss.displayPicture (800, 600) Gloss.black glossState 1.0


-- The game network
-- I wish I could avoid passing this gloss state
gameNetwork :: Gloss.State -> Event (SDL.Event) -> Event () -> MomentIO (Behavior (IO ()))
gameNetwork glossState eInput eUpdate = do
  bCounter <- accumB 0 $ unions [ (+1) <$ eInput ]
  let bGame = Game <$> bCounter

  return $ renderGame glossState . render <$> bGame


-- Render the game to a Gloss.Picture
render :: Game -> Gloss.Picture
render game =
  let count = counter game
    in Gloss.Pictures [ Gloss.Color Gloss.white $ Gloss.circle 10
                      , Gloss.Color Gloss.white $ Gloss.text (show count)
                      ]
