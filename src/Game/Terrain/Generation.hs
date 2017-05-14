-- | 
-- Module      :  Game.Terrain.Generation
-- Description :  Used to generate and manage terrain chunks
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Game.Terrain.Generation
  (
  )
where


import Game.Terrain.Data
import Data.Map               as Map


-- | A representation of a terrain made up of chunks of the given dimensions
-- Generates chunks as requested, loading pre-generated chunks if possible

data ITerrain = ITerrain { _dimensions :: (Int, Int, Int)
                         , _chunks :: Map.Map (Int, Int, Int) IChunk
                         }
