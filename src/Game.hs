module Game
  ( initialGameState
  , update
  , render
  )
where

import Prelude hiding ((.))

import Control.Wire hiding (id)
import FRP.Netwire.Move

import qualified Graphics.Gloss                  as Gloss


data Game = Game { _counter :: Double
                 }


-- A simple counter
counterWire :: (Monad m, HasTime t s) => Wire s e m a Double
counterWire = integral 0 . (pure 1)


-- The game state type, comprising of a session (fixed timestep), a main game wire, and the last game state
type GameSession = Timed NominalDiffTime ()
type GameWire e a = Wire GameSession e Identity a Game
type GameState x e a = (Session Identity GameSession, GameWire e a, Game)


-- The initial game state
initialGameState :: GameState x e a
initialGameState = let session = countSession 1 <*> pure ()
                       wire = Game <$> counterWire
                       -- This means if update doesn't get called before render it'll break
                       game = (\(_, _, g) -> g) (update (session, wire, undefined))
                    in (session, wire, game)


-- Update the game
update :: GameState Int e () -> GameState Int e ()
update (session, wire, _) = let (s, session') = runIdentity $ stepSession session
                                (r, wire') = runIdentity $ stepWire wire s (Right mempty)
                                -- Irrefutable if the game wire inhibits
                                Right game' = r
                            in (session', wire', game')


-- Render the game to a Gloss.Picture
render (_, _, game) =
  let count = _counter game
      (playerX, playerY) = (0, 0)
    in Gloss.Pictures [ Gloss.Color Gloss.white $ Gloss.translate (fromIntegral playerX) (fromIntegral playerY) $ Gloss.circle 10
                      , Gloss.Color Gloss.white $ Gloss.text (show count)
                      ]
