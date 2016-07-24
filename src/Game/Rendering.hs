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

import Control.Lens
import System.Random
import Game.Data
import Game.Terrain
import Game.Terrain.Rendering
import Data.Aeson
import Data.IORef
import Linear
import qualified SDL
import qualified Codec.Picture                   as Juicy
import qualified LambdaCube.GL                   as LC
import qualified LambdaCube.GL.Mesh              as LC
import qualified LambdaCube.Linear               as LC
import qualified Data.Map                        as Map
import qualified Data.Vector                     as V
import qualified Data.ByteString                 as BS
  

-- | Render the game based on its current state

renderGame :: IO (Game -> IO ())
renderGame = do

  -- Set up lambdacube
  let inputSchema = LC.makeSchema $ do
        LC.defObjectArray "objects" LC.Triangles $ do
            "position"        LC.@: LC.Attribute_V3F
            "uv"              LC.@: LC.Attribute_V2F
        LC.defUniforms $ do
            "viewMat"         LC.@: LC.M44F
            "time"            LC.@: LC.Float
            "diffuseTexture"  LC.@: LC.FTexture2D

  -- Allocate storage
  storage <- LC.allocStorage inputSchema

  -- Upload mesh data
  --uploadMeshToGPU triangleA >>= addMeshToObjectArray storage "objects" []
  --uploadMeshToGPU triangleB >>= addMeshToObjectArray storage "objects" []
  --uploadMeshToGPU box >>= addMeshToObjectArray storage "objects" []
  
  -- Load texture
  Right img <- Juicy.readImage "data/patchouli.png"
  textureData <- LC.uploadTexture2DToGPU img

  -- Load pipeline and generate renderer
  Just pipelineDesc <- decodeStrict <$> BS.readFile "data/pipelines/3d.json"
  renderer <- LC.allocRenderer pipelineDesc

  -- Generate some random terrain
  terrain <- randomChunk (15, 15, 15)
  let terrainMesh = genChunkMesh terrain
  LC.uploadMeshToGPU terrainMesh >>= LC.addMeshToObjectArray storage "objects" []

  -- Set storage
  LC.setStorage renderer storage

  -- Get start ticks
  start <- SDL.ticks

  -- An IORef for storing the ticks value
  lastTicksRef <- newIORef start

  -- The render handler to return
  return $ \game -> do
    --let Linear.V3 cx cy cz = _camPos game
    --let viewMat = convertMatrix (Linear.mkTransformation (_camRot game) (_camPos game))
    --let viewMat = convertMatrix (Linear.mkTransformationMat (Linear.identity) (_camPos game))
    let mTranslation = identity & translation .~ (-(_camPos game)) :: M44 Float
    let mRotation = m33_to_m44 . fromQuaternion $ _camRot game :: M44 Float
    let viewMat = convertMatrix $ (mRotation !*! mTranslation)

    lastTicks <- readIORef lastTicksRef
    ticks <- SDL.ticks
    writeIORef lastTicksRef ticks

    let msPassed = ticks - lastTicks

    -- crappy fps counter that only works <1000 fps
    --print (1000 / fromIntegral msPassed)

    let diff = (fromIntegral (ticks - start)) / 100
    let time = (fromIntegral ticks / 2000)

    LC.setScreenSize storage 800 600
    LC.updateUniforms storage $ do
      "viewMat"        LC.@= return viewMat
      "diffuseTexture" LC.@= return textureData
      "time"           LC.@= return (time :: Float)

    LC.renderFrame renderer


-- | Convert a linear matrix to a lambacube one..

convertMatrix :: M44 Float -> LC.M44F
convertMatrix mat = let V4 (V4 a b c d)
                           (V4 e f g h)
                           (V4 i j k l)
                           (V4 m n o p) = mat
                    in LC.V4 (LC.V4 a e i m)
                             (LC.V4 b f j n)
                             (LC.V4 c g k o)
                             (LC.V4 d h l p)
