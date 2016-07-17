{-# LANGUAGE LambdaCase #-}

module Game
  ( gameNetwork
  )
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified SDL.Event                       as SDL
import qualified SDL.Input.Keyboard              as SDL
import qualified SDL.Input.Keyboard.Codes        as SDL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss


data Game = Game { counter :: Int
                 , pos :: (Int, Int)
                 }

-- Function to render a gloss picture
renderGame glossState = Gloss.displayPicture (800, 600) Gloss.black glossState 1.0


-- The game network
-- I wish I could avoid passing this gloss state
gameNetwork :: Gloss.State -> Event (SDL.Event) -> Event () -> MomentIO (Behavior (IO ()))
gameNetwork glossState eInput eUpdate = do
  let filterKeyEvent e = (\case SDL.KeyboardEvent _ -> True; _ -> False) $ SDL.eventPayload e
  let filterKey scancode (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym eScan _ _)))) = eScan == scancode

  let keyEvents = filterE filterKeyEvent eInput
  let leftKeyEvent  = filterE (filterKey SDL.ScancodeLeft)  keyEvents
  let rightKeyEvent = filterE (filterKey SDL.ScancodeRight) keyEvents
  let downKeyEvent  = filterE (filterKey SDL.ScancodeDown)  keyEvents
  let upKeyEvent    = filterE (filterKey SDL.ScancodeUp)    keyEvents

  playerX <- accumB (0 :: Int) $ unions [ subtract 9 <$ leftKeyEvent
                                        , (+9)       <$ rightKeyEvent ]
  playerY <- accumB (0 :: Int) $ unions [ subtract 9 <$ downKeyEvent
                                        , (+9)       <$ upKeyEvent ]

  let bPos = (,) <$> playerX <*> playerY

  -- Count input events
  bCounter <- accumB 0 $ unions [ (+1) <$ keyEvents ]

  -- Compose game behaviour
  let bGame = Game <$> bCounter <*> bPos

  -- Compose game rendering
  return $ renderGame glossState . render <$> bGame


-- Render the game to a Gloss.Picture
render :: Game -> Gloss.Picture
render game =
  let count = counter game
      (playerX, playerY) = pos game
    in Gloss.Pictures [ Gloss.Color Gloss.white $ Gloss.translate (fromIntegral playerX) (fromIntegral playerY) $ Gloss.circle 10
                      , Gloss.Color Gloss.white $ Gloss.text (show count)
                      ]
