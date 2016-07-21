-- | 
-- Module      :  Game.Simulation
-- Description :  The main entry into the game's simulation
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Game.Simulation
  ( gameNetwork
  )
where

import Framework
import Game.Data
import Game.Simulation.Input
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified SDL.Event          as SDL
import qualified SDL.Input.Keyboard as SDL


-- | The overall game behavior, which takes input and tick events
-- and produces a Game
gameNetwork :: InputEvent -> TickEvent -> MomentIO (Behavior Game)
gameNetwork eInput eTick = do
  -- Current time as an event
  eTime <- accumE 0 ((+) <$> eTick)

  -- Current time as a behavior
  bTime <- stepper 0 eTime

  bAPressed <- keyDown eInput SDL.ScancodeA

  --bCount <- accumB 0 $ (+1) <$ keyReleased eInput SDL.ScancodeA
  let bCount = fmap (\b -> if b then 1 else 0) bAPressed

  -- The overall game behavior
  return $ Game <$> bTime <*> pure (0, 0)
