-- | 
-- Module      :  Game.Data
-- Description :  Defines the main datatype for the data of the game
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Game.Data
  ( Game(..)
  )
where

import Linear


-- | The main Game data type.
-- This contains all of the game data, preferably broken
-- down into sub-types so as to remain modular.
-- The constructor of this type is used to construct a
-- reactive-banana behavior, the network of which comprises
-- the behavior of the game.

data Game = Game { _camViewMat :: M44 Float
                 }
