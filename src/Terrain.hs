-- | 
-- Module      :  Terrain
-- Description :  The entry for the terrain rendering library
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Terrain
  ( module Terrain.Data
  , module Terrain.Rendering
  , randomChunk
  )
where

import Terrain.Data
import Terrain.Rendering
import System.Random
import qualified Data.Vector.Unboxed as VU


-- | Generate some random terrain

randomChunk :: (Int, Int, Int) -> IO (IChunk)
randomChunk (dx, dy, dz) =
  (IChunk (dx, dy, dz)) <$> VU.replicateM (dx * dy * dz) (randomRIO (True, False))
