{-# LANGUAGE LambdaCase #-}

module Game.Simulation.Camera.FPSCamera
  ( fpsCamera
  , viewMatrix
  )
where

import Linear
import Framework
import Control.Lens
import Game.Simulation.Input
import Reactive.Banana
import Reactive.Banana.Frameworks
import Game.Simulation.Camera.Camera

import qualified SDL.Event          as SDL
import qualified SDL.Input.Keyboard as SDL


type InitialPos = V3 Float
type InitialRot = V2 Float -- ^ X and Y rotation angle in degrees
type NearVal = Float
type FOV = Float
type Aspect = Float


-- | An FPS camera

fpsCamera :: InputEvent
          -> TickEvent
          -> InitialPos
          -> InitialRot
          -> FOV
          -> Aspect
          -> NearVal
          -> MomentIO (Behavior Camera)
fpsCamera eInput eTick initialPos initialRot eFov eAspect eNear = do
  bRDown <- keyDown eInput SDL.ScancodeR
  bTDown <- keyDown eInput SDL.ScancodeT
  let bFovChange = fovChange <$> bRDown <*> bTDown
  let eFovChange = ((*) <$> bFovChange) <@> eTick
  bCamFov <- accumB eFov ((+) <$> eFovChange)

  let bProjection = infinitePerspective <$> bCamFov <*> pure eAspect <*> pure eNear

  bCamRot <- camOrientation eInput initialRot
  bCamPos <- positionCamera eInput eTick initialPos bCamRot

  let bViewMatrix = viewMatrix <$> bCamPos <*> bCamRot
  let bMvpMatrix = (!*!) <$> bProjection <*> bViewMatrix

  return $ Camera <$> bProjection <*> bViewMatrix <*> bMvpMatrix <*> bCamPos <*> bCamRot <*> bCamFov


-- | Convert a translation matrix and a quaternion to a view matrix

viewMatrix :: V3 Float -> Quaternion Float -> M44 Float
viewMatrix camTranslation camOrientation =
    -- Camera translation is negated since view matrix = inverse
    -- of camera matrix
    let mTranslation = identity & translation .~ (-camTranslation)
        mRotation = m33_to_m44 . fromQuaternion $ camOrientation
    in mRotation !*! mTranslation


-- | A behavior describing the camera position
positionCamera :: InputEvent
               -> TickEvent
               -> V3 Float
               -> Behavior (Quaternion Float)
               -> MomentIO (Behavior (V3 Float))
positionCamera eInput eTick initialPos bCamRot = do
  let speed = 0.1

  forward <- keyDown eInput SDL.ScancodeW
  back <- keyDown eInput SDL.ScancodeS
  left <- keyDown eInput SDL.ScancodeA
  right <- keyDown eInput SDL.ScancodeD
  down <- keyDown eInput SDL.ScancodeQ
  up <- keyDown eInput SDL.ScancodeE

  let eMouseMoved = mouseMoved eInput Absolute

  let bVelocity = (camVelocity speed <$> bCamRot <*> forward
                                     <*> back <*> left <*> right
                                     <*> down <*> up)

  let bAddVelocity = (+) <$> bVelocity
  let eAddVelocity = bAddVelocity <@ eTick

  bCamPos <- accumB initialPos eAddVelocity

  return bCamPos


-- | Calculates the camera velocity based on speed and inputs
-- The (bool, bool, bool, bool) is (up, down, left, right)

camVelocity :: Float -> Quaternion Float -> Bool
            -> Bool -> Bool -> Bool -> Bool -> Bool -> V3 Float
camVelocity speed camRot forward back left right down up =
  let forwardVel True False  = -speed
      forwardVel False True  =  speed
      forwardVel _     _     =  0
      horzVel    True  False = -speed
      horzVel    False True  =  speed
      horzVel    _     _     =  0
      vertVel    True  False =  speed
      vertVel    False True  = -speed
      vertVel    _     _     =  0
  in V3 (horzVel left right)
        (vertVel up down)
        (forwardVel forward back) *! fromQuaternion camRot


-- | A behavior describing the camera orientation
camOrientation :: InputEvent -> InitialRot -> MomentIO (Behavior (Quaternion Float))
camOrientation eInput initialRot = do
  bMouseDown <- mouseButtonDown eInput SDL.ButtonLeft

  let eMouseMove = (/300) <$> whenE bMouseDown (mouseMoved eInput Delta)

  bCameraRotationComponents <- accumB initialRot ((+) <$> eMouseMove)

  return $ cameraRotation <$> bCameraRotationComponents


-- | Update camera orientation quaternion based on a mouse movement
-- It basically converts from eulers to axisAngle, and could be abstracted

cameraRotation :: V2 Float -> Quaternion Float
cameraRotation (V2 yaw pitch) = axisAngle (V3 1 0 0) (pitch)
                              * axisAngle (V3 0 1 0) (yaw)


-- | The change in fov per second according to whether the buttons are down

fovChange :: Bool -> Bool -> Float
fovChange False True =  0.5
fovChange True False = -0.5
fovChange _ _ = 0.0
