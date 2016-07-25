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

import Linear
import Framework
import Control.Lens
import Game.Data
import Game.Simulation.Input
import Game.Simulation.Camera.FPSCamera
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

  -- FPS camera
  bViewMat <- fpsCamera eInput eTick (V3 20 15 20) (V2 (-45) 0) 90 1 0.1

  -- The overall game behavior
  return $ Game <$> bViewMat


