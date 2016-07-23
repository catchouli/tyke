-- | 
-- Module      :  Terrain.Rendering
-- Description :  Responsible for generating meshes from terrain
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Terrain.Rendering
where

import Terrain.Data
import LambdaCube.GL
import LambdaCube.GL.Mesh
import Data.Foldable
import Debug.Trace
import qualified Data.Map                        as Map
import qualified Data.Vector                     as V
import qualified Data.Vector.Unboxed             as VU


-- | Generates a mesh from a terrain chunk

genChunkMesh :: IChunk -> Mesh
genChunkMesh chunk@(IChunk dimensions@(dx, dy, dz) blocks) =
  let
    -- Generate a big list of indices for each block in the chunk
    -- Not!! the 3d rendering kind of indices
    indices = [(x,y,z) | x <- [0..dx-1], y <- [0..dy-1], z <- [0..dz-1]]
    -- Fold over block indices and generate faces for each block
    vertices = foldl' (genBlockMesh chunk) (V.fromList []) indices
    -- Get the number of faces (sketchy if I change the format)
    faceCount = trace (show $ V.length vertices) $ V.length vertices `div` 6
    -- Generate uvs (just simiple box uvs.. im not fussed the order right now)
    uvs = concat . replicate faceCount $ [ V2 0 0, V2 1 0, V2 0 1
                                         , V2 0 1, V2 1 0, V2 1 1 ]
  in Mesh
      { mAttributes   = Map.fromList
            [ ("position",  A_V3F vertices )
            , ("uv",        A_V2F $ V.fromList uvs )
            ]
      , mPrimitive = P_Triangles
      }


-- | Generates faces for a single block in a chunk

genBlockMesh :: IChunk
             -> V.Vector (V3 Float)
             -> (Int, Int, Int)
             -> V.Vector (V3 Float)
genBlockMesh chunk vertices idx =
  (V.++) vertices (foldl' (genBlockFace chunk idx) (V.fromList []) cardinalDirections)


-- | Generates a face for a block, if applicable

genBlockFace :: IChunk
             -> (Int, Int, Int)
             -> V.Vector (V3 Float)
             -> (Int, Int, Int)
             -> V.Vector (V3 Float)
genBlockFace (IChunk dimensions@(dx, dy, dz) blocks) pos@(x, y, z) vertices dir@(dirx, diry, dirz) =
  let defaultFace :: [ V2 Float ]
      min = -0.5
      max = 0.5
      defaultFace = [ V2 min min, V2 max min, V2 min max,
                      V2 min max, V2 max min, V2 max max ]
      vertexFun :: V2 Float -> V3 Float
      vertexFun (V2 x y) = case dir of (1, 0, 0)  -> V3 0.5 x y
                                       (-1, 0, 0) -> V3 (-0.5) x (-y)
                                       (0, 1, 0)  -> V3 (-x) 0.5 y
                                       (0, -1, 0) -> V3 x (-0.5) y
                                       (0, 0, 1)  -> V3 x y 0.5
                                       (0, 0, -1) -> V3 (-x) y (-0.5)
      addOffset :: V3 Float -> V3 Float
      addOffset (V3 x' y' z') = V3 ((fromIntegral x)+x'-(fromIntegral dx/2)) ((fromIntegral y)+y'-(fromIntegral dy/2)) ((fromIntegral z)+z'-(fromIntegral dz/2))
      vectorIndex = posToIdx dimensions pos
      block = VU.unsafeIndex blocks vectorIndex
   in if block
         then (V.++) vertices (V.fromList $ map (addOffset . vertexFun) defaultFace)
         else vertices

-- | The 6 cardinal directions
cardinalDirections = [ (1, 0, 0), (-1, 0, 0)
                     , (0, 1, 0), (0, -1, 0)
                     , (0, 0, 1), (0, 0, -1)
                     ]

-- | Whether a set of coordinates is in range
inRange :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
inRange (dx, dy, dz) (x, y, z) = x >= 0 && x < dx
                              && y >= 0 && y < dx
                              && z >= 0 && z < dz


-- | Converts a (x,y,z) index to a vector index
posToIdx :: (Int, Int, Int) -> (Int, Int, Int) -> Int
posToIdx (dx, dy, dz) (x,y,z) = x + dx * (y + dy * z)
