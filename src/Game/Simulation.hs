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

import Game.Data
import Framework
import Reactive.Banana
import Reactive.Banana.Frameworks


-- | The overall game behavior, which takes input and tick events
-- and produces a Game
gameNetwork :: InputEvent -> TickEvent -> MomentIO (Behavior Game)
gameNetwork eInput eTick = do
  -- Current time as an event
  eTime <- accumE 0 ((+) <$> eTick)

  -- Current time as a behavior
  bTime <- stepper 0 eTime

  -- The overall game behavior
  return $ Game <$> bTime
