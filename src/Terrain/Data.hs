{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-- | 
-- Module      :  Terrain.Data
-- Description :  Defines the data structures for the terrain
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Terrain.Data
where

import Data.SafeCopy
import System.Random
import Data.Serialize.Get
import Data.Serialize.Put

import qualified Data.Vector.Unboxed as VU


-- | The immutable terrain chunk datatype and its safecopy instance

data IChunk = IChunk { dimensions :: (Int, Int, Int)
                     , blocks :: VU.Vector Bool
                     }
                     deriving Show

instance SafeCopy IChunk where
  putCopy (IChunk dimensions blocks) = contain $ safePut dimensions >> safePut blocks
  getCopy = contain $ IChunk <$> safeGet <*> safeGet
