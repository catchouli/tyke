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
import qualified LambdaCube.Linear as LC
import LambdaCube.GL
import LambdaCube.GL.Mesh
import Data.Aeson
import Codec.Picture
import System.Random
import Game.Terrain
import Game.Terrain.Rendering
import Data.IORef
import Data.Coerce
import qualified Linear.V3                       as Linear
import qualified Linear.V4                       as Linear
import qualified Linear.Matrix                   as Linear
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
            "viewMat"         @: M44F
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

  lastTicksRef <- newIORef start

  -- The render handler to return
  return $ \game -> do
    let Linear.V3 cx cy cz = _camPos game
    let viewMat = convertMatrix (Linear.mkTransformation (_camRot game) (_camPos game))
    --let viewMat = convertMatrix (Linear.mkTransformationMat (Linear.identity) (_camPos game))

    lastTicks <- readIORef lastTicksRef
    ticks <- SDL.ticks
    writeIORef lastTicksRef ticks

    let msPassed = ticks - lastTicks

    -- crappy fps counter that only works <1000 fps
    --print (1000 / fromIntegral msPassed)

    let diff = (fromIntegral (ticks - start)) / 100
    let time = (fromIntegral ticks / 2000)

    setScreenSize storage 800 600
    updateUniforms storage $ do
      "viewMat" @= return viewMat
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


-- | Convert a linear matrix to a lambacube one..

convertMatrix :: Linear.M44 Float -> LC.M44F
convertMatrix mat = let Linear.V4 (Linear.V4 a b c d)
                                  (Linear.V4 e f g h)
                                  (Linear.V4 i j k l)
                                  (Linear.V4 m n o p) = mat
                  in V4 (V4 a e i m)
                        (V4 b f j n)
                        (V4 c g k o)
                        (V4 d h l p)
