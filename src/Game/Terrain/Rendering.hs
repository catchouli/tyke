-- | 
-- Module      :  Game.Terrain.Rendering
-- Description :  Responsible for generating meshes from terrain
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Game.Terrain.Rendering
where

import Game.Terrain.Data
import LambdaCube.GL
import LambdaCube.GL.Mesh
import Data.Foldable
import Debug.Trace
import qualified Data.Map                        as Map
import qualified Data.Vector                     as V
import qualified Data.Vector.Unboxed             as VU


-- Tuple accessors

t3_1 (a, _, _) = a -- ^ Access the first element of a 3-tuple
t3_2 (_, b, _) = b -- ^ Access the second element of a 3-tuple
t3_3 (_, _, c) = c -- ^ Access the third element of a 3-tuple


-- | Generates a mesh from a terrain chunk

genChunkMesh :: IChunk -> Mesh
genChunkMesh chunk@(IChunk dimensions@(dx, dy, dz) blocks) =
  let
    -- Generate a big list of indices for each block in the chunk
    -- Not!! the 3d rendering kind of indices
    indices = [(x,y,z) | x <- [0..dx-1], y <- [0..dy-1], z <- [0..dz-1]]
    -- Fold over block indices and generate faces for each block
    blockMeshes = map (genBlockMesh chunk) indices
    -- Get vertices and concatenate them into a single vector
    vertices = V.concat . map t3_1 $ blockMeshes
    -- Get uvs and concatenate them into a single vector
    uvs = V.concat . map t3_2 $ blockMeshes
    -- Get normals and concatenate them into a single vector
    normals = V.concat . map t3_3 $ blockMeshes
  in Mesh
      { mAttributes   = Map.fromList
            [ ( "position",  A_V3F vertices )
            , ( "uv",        A_V2F uvs )
            , ( "normal",    A_V3F normals )
            ]
      , mPrimitive = P_Triangles
      }


-- | Generates faces for a single block in a chunk

genBlockMesh :: IChunk
             -> (Int, Int, Int)
             -> ( V.Vector (V3 Float)
                , V.Vector (V2 Float)
                , V.Vector (V3 Float) )
genBlockMesh chunk@(IChunk dimensions blocks) idx =
  let vectorIndex = posToIdx dimensions idx
      block = VU.unsafeIndex blocks vectorIndex
      genFace = genBlockFace chunk idx
      faces = map genFace cardinalDirections
  in if block
        then (V.concat . map t3_1 $ faces,
              V.concat . map t3_2 $ faces,
              V.concat . map t3_3 $ faces)
        else (V.empty, V.empty, V.empty)


-- | Generates a face for a block, if applicable

genBlockFace :: IChunk
             -> (Int, Int, Int)
             -> (Int, Int, Int)
             -> ( V.Vector (V3 Float)
                , V.Vector (V2 Float)
                , V.Vector (V3 Float) )
genBlockFace (IChunk dimensions@(dx, dy, dz) blocks) 
             pos@(x, y, z) dir@(dirx, diry, dirz) =  
  let min = -0.5                                     
      max = 0.5                                      
      defaultFace = [ V2 min min, V2 max min, V2 min max
                    , V2 min max, V2 max min, V2 max max ]
      posF = fromIntegral <$> V3 x y z
      -- A constant 0.5 offset is added or the centre of
      -- block (0,0,0) would be at the origin
      addOffset (V3 a b c)
                (V3 d e f) = V3 (a+d+0.5) (b+e+0.5) (c+f+0.5)
      -- The block adjacent to the current one (in dir)
      -- If it's air, then we should generate a face so there's no holes
      -- in the mesh
      adjBlockPos = (x+dirx, y+diry, z+dirz)
      adjBlockIdx = posToIdx dimensions adjBlockPos
      adjBlock = if inRange dimensions adjBlockPos
                    then VU.unsafeIndex blocks adjBlockIdx
                    else False
      transformVertex = addOffset posF . cubeVertex dir
      normal = fromIntegral <$> V3 dirx diry dirz
   in if not adjBlock
         then let vertices = V.fromList (map transformVertex defaultFace)
                  uvs = V.fromList $ cubeFaceUvs
                  normals = V.fromList $ replicate 6 normal
              in (vertices, uvs, normals)
         else (V.empty, V.empty, V.empty)


-- | Converts a 2d vertex on the face of a cube to a 3d vertex
-- based on the direciton of the face from the centre of the cube

cubeVertex :: (Int, Int, Int) -> V2 Float -> V3 Float
cubeVertex dir (V2 x y) = case dir of ( 1,  0,  0) -> V3 0.5 y (-x)
                                      (-1,  0,  0) -> V3 (-0.5) y x
                                      ( 0,  1,  0) -> V3 (-x) 0.5 y
                                      ( 0, -1,  0) -> V3 x (-0.5) y
                                      ( 0,  0,  1) -> V3 x y 0.5
                                      ( 0,  0, -1) -> V3 (-x) y (-0.5)

-- | Convert from a 

cubeFaceUvs :: [V2 Float]
cubeFaceUvs = [ V2 0 1, V2 1 1, V2 0 0, V2 0 0, V2 1 1, V2 1 0 ]


-- | The 6 cardinal directions
cardinalDirections = [ (1, 0, 0), (-1, 0, 0)
                     , (0, 1, 0), (0, -1, 0)
                     , (0, 0, 1), (0, 0, -1)
                     ]

-- | Whether a set of coordinates is in range
inRange :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
inRange (dx, dy, dz) (x, y, z) = x >= 0 && x < dx
                              && y >= 0 && y < dy
                              && z >= 0 && z < dz


-- | Converts a (x,y,z) index to a vector index
posToIdx :: (Int, Int, Int) -> (Int, Int, Int) -> Int
posToIdx (dx, dy, dz) (x,y,z) = x + dx * (y + dy * z)
