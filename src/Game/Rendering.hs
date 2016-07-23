{-# LANGUAGE OverloadedStrings #-}

-- | 
-- Module      :  Game.Rendering
-- Description :  Exposes the renderGame function and is the 
--                entry point into the main rendering path for the game
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Game.Rendering
  ( renderGame
  )
where

import Game.Data
import LambdaCube.GL
import LambdaCube.GL.Mesh
import Data.Aeson
import Codec.Picture
import Data.Array.Unboxed as UArray
import System.Random
import Terrain
import Terrain.Rendering
import qualified SDL
import qualified Data.Map                        as Map
import qualified Data.Vector                     as V
import qualified Data.ByteString                 as BS
  

-- | Render the game based on its current state

renderGame :: IO (Game -> IO ())
renderGame = do

  -- Set up lambdacube
  let inputSchema = makeSchema $ do
        defObjectArray "objects" Triangles $ do
            "position"        @: Attribute_V3F
            "uv"              @: Attribute_V2F
        defUniforms $ do
            "pos"             @: V3F
            "time"            @: Float
            "diffuseTexture"  @: FTexture2D

  -- Allocate storage
  storage <- allocStorage inputSchema

  -- Upload mesh data
  --uploadMeshToGPU triangleA >>= addMeshToObjectArray storage "objects" []
  --uploadMeshToGPU triangleB >>= addMeshToObjectArray storage "objects" []
  --uploadMeshToGPU box >>= addMeshToObjectArray storage "objects" []
  
  -- Load texture
  Right img <- readImage "data/patchouli.png"
  textureData <- uploadTexture2DToGPU img

  -- Load pipeline and generate renderer
  Just pipelineDesc <- decodeStrict <$> BS.readFile "data/pipelines/3d.json"
  renderer <- allocRenderer pipelineDesc

  -- Generate some random terrain
  terrain <- randomChunk (15, 15, 15)
  let terrainMesh = genChunkMesh terrain
  uploadMeshToGPU terrainMesh >>= addMeshToObjectArray storage "objects" []

  -- Set storage
  setStorage renderer storage

  -- Get start ticks
  start <- SDL.ticks

  -- The render handler to return
  return $ \game -> do
    ticks <- SDL.ticks

    let diff = (fromIntegral (ticks - start)) / 100
    let time = (fromIntegral ticks / 2000)

    setScreenSize storage 800 600
    updateUniforms storage $ do
      "pos" @= return (V3 0 0 (55) :: V3 Float)
      "diffuseTexture" @= return textureData
      "time" @= return (time :: Float)

    renderFrame renderer


-- | Generate a box

box :: Mesh
box = let faces = [minBound .. maxBound] :: [BoxFace]
          vertices = concatMap genBoxFace faces
          uvs = concat . replicate 6 $ [V2 0 0, V2 1 0, V2 0 1, V2 0 1, V2 1 0, V2 1 1]
      in Mesh
          { mAttributes   = Map.fromList
              [ ("position",  A_V3F $ V.fromList vertices )
              , ("uv",        A_V2F $ V.fromList uvs )
              ]
          , mPrimitive = P_Triangles
          }

-- | A data type describing the current face of a box

data BoxFace = PositiveX | NegativeX | PositiveY | NegativeY | PositiveZ | NegativeZ deriving (Show, Enum, Bounded)


faceFun :: BoxFace -> V2 Float -> V3 Float
faceFun PositiveX = \(V2 y z) -> V3 0.5 y z
faceFun NegativeX = \(V2 y z) -> V3 (-0.5) y (-z)
faceFun PositiveY = \(V2 x z) -> V3 (-x) 0.5 z
faceFun NegativeY = \(V2 x z) -> V3 x (-0.5) z
faceFun PositiveZ = \(V2 x y) -> V3 x y 0.5
faceFun NegativeZ = \(V2 x y) -> V3 (-x) y (-0.5)

-- | Generate the face of a box

genBoxFace :: BoxFace -> [V3 Float]
genBoxFace boxFace = let face (min, max) = [ V2 min min, V2 max min, V2 min max, V2 min max, V2 max min, V2 max max ]
                         vertices = map (faceFun boxFace) (face (-0.5, 0.5))
                     in vertices


-- | A sample triangle

triangleA :: Mesh
triangleA = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ V.fromList [V2 1 1, V2 1 (-1), V2 (-1) (-1)])
        , ("uv",        A_V2F $ V.fromList [V2 1 1, V2 0 1, V2 0 0])
        ]
    , mPrimitive    = P_Triangles
    }


-- | A sample triangle

triangleB :: Mesh
triangleB = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ V.fromList [V2 1 1, V2 (-1) (-1), V2 (-1) 1])
        , ("uv",        A_V2F $ V.fromList [V2 1 1, V2 0 0, V2 1 0])
        ]
    , mPrimitive    = P_Triangles
    }
