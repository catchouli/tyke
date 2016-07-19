{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game
  ( initialGameState
  , update
  , render
  )
where

import Prelude hiding ((.))

import Control.Wire hiding (id)
import FRP.Netwire.Move

import qualified SDL.Event                       as SDL
import qualified SDL.Input.Keyboard              as SDL
import qualified SDL.Input.Keyboard.Codes        as SDL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss


data Game = Game { _counter :: Double
                 }


-- A simple counter
counterWire :: (Monad m, HasTime t s) => Wire s e m a Double
counterWire = integral 0 . 1


-- The game state type, comprising of a session (fixed timestep), a main game wire, and the last game state
type GameState x e a = (Session Identity (Timed NominalDiffTime ()), Wire (Timed NominalDiffTime ()) e Identity a Game, Game)


-- Function to render a gloss picture
initialGameState :: GameState x e a
initialGameState = let session = countSession 1 <*> pure ()
                       wire = Game <$> counterWire
                       game = Game 0.0
                    in (session, wire, game)


-- Update the game
--update :: GameState x e -> GameState x e
update :: GameState Int e () -> GameState Int e ()
update (session, wire, _) = let (s, session') = runIdentity $ stepSession session
                                (r, wire') = runIdentity $ stepWire wire s (Right mempty)
                                -- Irrefutable if the game wire inhibits
                                Right game' = r
                            in (session', wire', game')


-- Render the game to a Gloss.Picture
--render :: GameState x e -> Gloss.Picture
render (_, _, game) =
  let 
      count = _counter game
      (playerX, playerY) = (0, 0)
    in Gloss.Pictures [ Gloss.Color Gloss.white $ Gloss.translate (fromIntegral playerX) (fromIntegral playerY) $ Gloss.circle 10
                      , Gloss.Color Gloss.white $ Gloss.text (show count)
                      ]
