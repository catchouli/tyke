-- | 
-- Module      :  Game
-- Description :  Exposes gameNetwork and renderGame and provides
--                an entry point to the game modules
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Game
  ( module Game.Simulation
  , module Game.Rendering
  )
where

import Game.Simulation (gameNetwork)
import Game.Rendering (renderGame)
