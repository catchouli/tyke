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
import Graphics.Rendering.FTGL
import Graphics.GL.Compatibility33
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
            "normal"          LC.@: LC.Attribute_V3F
        LC.defUniforms $ do
            "viewMat"         LC.@: LC.M44F
            "time"            LC.@: LC.Float
            "diffuseTexture"  LC.@: LC.FTexture2D

  -- Allocate storage
  storage <- LC.allocStorage inputSchema
  
  -- Load texture
  Right img <- Juicy.readImage "data/patchouli.png"
  textureData <- LC.uploadTexture2DToGPU img

  -- Load pipeline and generate renderer
  Just pipelineDesc <- decodeStrict <$> BS.readFile "data/pipelines/3d.json"
  renderer <- LC.allocRenderer pipelineDesc

  -- Generate some random terrain
  terrain <- randomChunk (15, 15, 15)
  let terrainMesh = genChunkMesh terrain
  LC.uploadMeshToGPU terrainMesh >>=
    LC.addMeshToObjectArray storage "objects" []

  -- Set storage
  LC.setStorage renderer storage

  -- Load font
  font <- createTextureFont "data/fonts/droidsans.ttf"
  setFontFaceSize font 22 72

  -- Get start ticks
  start <- SDL.ticks

  -- An IORef for storing the ticks value
  lastFPSUpdateRef <- newIORef start
  lastFPSAvgRef <- newIORef (60 :: Int) -- ^ the initial value is a lie :)
  lastFPSFrameCount <- newIORef (0 :: Int)

  -- The render handler to return
  return $ \game -> do
    -- Calculate view matrix
    let mTranslation = identity & translation .~ (-(_camPos game))
    let mRotation = m33_to_m44 . fromQuaternion $ _camRot game
    let viewMat = convertMatrix $ (mRotation !*! mTranslation)

    -- Update timer
    ticks <- SDL.ticks

    -- Update fps counter
    modifyIORef lastFPSFrameCount (+1)
    lastFPSUpdate <- readIORef lastFPSUpdateRef

    if (ticks - lastFPSUpdate) >= 1000
       then do avgFps <- readIORef lastFPSFrameCount
               writeIORef lastFPSAvgRef avgFps
               writeIORef lastFPSUpdateRef ticks
               writeIORef lastFPSFrameCount 0
       else return ()

    -- Read average fps
    fps <- readIORef lastFPSAvgRef

    -- Calculate time for unions
    let diff = (fromIntegral (ticks - start)) / 100
    let time = (fromIntegral ticks / 1000)

    -- Update uniforms
    LC.setScreenSize storage 800 600
    LC.updateUniforms storage $ do
      "viewMat"        LC.@= return viewMat
      "diffuseTexture" LC.@= return textureData
      "time"           LC.@= return (time :: Float)

    -- Render a frame
    LC.renderFrame renderer

    -- Render debug tex
    renderStats font fps

-- | Render stats

renderStats :: Font -> Int -> IO ()
renderStats font fps = do
  -- Clean up opengl state (after lc render)

  glUseProgram 0
  glColor3f 1 1 1
  glLoadIdentity
  glOrtho 0 800 0 600 0 10

  -- Disable depth test so we can draw on top of lc render
  glDisable GL_DEPTH_TEST

  -- Draw text
  glPushMatrix

  glTranslatef 50 50 0
  renderFont font ("FPS: " ++ show fps ++ "hz") All
  
  glPopMatrix

  -- Restore depth test it's probably important
  glEnable GL_DEPTH_TEST


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
