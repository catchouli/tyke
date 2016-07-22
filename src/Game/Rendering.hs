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
import qualified SDL
import qualified Data.Map                        as Map
import qualified Data.Vector                     as V
import qualified Data.ByteString                 as BS
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss
  

-- | Render the game based on its current state

renderGame :: IO (Game -> IO ())
renderGame = do

  -- Set up lambdacube
  let inputSchema = makeSchema $ do
        defObjectArray "objects" Triangles $ do
            "position"        @: Attribute_V2F
            "uv"              @: Attribute_V2F
        defUniforms $ do
            "pos"             @: V3F
            "time"            @: Float
            "diffuseTexture"  @: FTexture2D

  -- Allocate storage
  storage <- allocStorage inputSchema

  -- Upload mesh data
  uploadMeshToGPU triangleA >>= addMeshToObjectArray storage "objects" []
  uploadMeshToGPU triangleB >>= addMeshToObjectArray storage "objects" []
  
  -- Load texture
  Right img <- readImage "data/patchouli.png"
  textureData <- uploadTexture2DToGPU img

  -- Load pipeline and generate renderer
  Just pipelineDesc <- decodeStrict <$> BS.readFile "data/pipelines/hello.json"
  renderer <- allocRenderer pipelineDesc

  -- Set storage
  setStorage renderer storage

  start <- SDL.ticks

  -- The render handler to return
  return $ \game -> do
    ticks <- SDL.ticks

    let diff = (fromIntegral (ticks - start)) / 100
    let time = (fromIntegral ticks / 200)

    setScreenSize storage 800 600
    updateUniforms storage $ do
      "pos" @= return (V3 0 0 (sin diff + 5) :: V3 Float)
      "diffuseTexture" @= return textureData
      "time" @= return (time :: Float)

    renderFrame renderer


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
