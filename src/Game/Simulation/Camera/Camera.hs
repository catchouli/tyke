{-# LANGUAGE TemplateHaskell #-}

module Game.Simulation.Camera.Camera where

import Linear
import Control.Lens

-- | Base camera data

data Camera = Camera { _camProjMat :: M44 Float
                     , _camViewMat :: M44 Float
                     , _camMVPMat :: M44 Float
                     , _camPosition :: V3 Float
                     , _camRotation :: Quaternion Float
                     , _camFov :: Float
                     }
makeLenses ''Camera
