module Game.Simulation.Camera.IsometricCamera
  ( isometricCamera
  )
where

import Linear
import Framework
import Control.Lens
import Game.Simulation.Input
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified SDL.Event          as SDL
import qualified SDL.Input.Keyboard as SDL


type InitialPos = V3 Float
type InitialRot = V2 Float -- ^ X and Y rotation angle in degrees
type NearVal = Float
type FOV = Float
type Aspect = Float


-- | An isometric camera

isometricCamera :: InputEvent
                -> TickEvent
                -> InitialPos
                -> MomentIO (Behavior (M44 Float))
isometricCamera eInput eTick initialPos = do
  let projection = ortho (-10.0) 10.0 (-10.0) 10.0 (-1000.0) 1000.0

  let camRot = cameraRotation (V2 (-45.0) 45.0)

  let bCamRot = pure camRot
  bCamPos <- camPosition eInput eTick initialPos bCamRot

  let bViewMatrix = viewMatrix <$> bCamPos <*> bCamRot
  let bMvpMatrix = (!*!) projection <$> bViewMatrix

  return $ bMvpMatrix


-- | Convert a translation matrix and a quaternion to a view matrix

viewMatrix :: V3 Float -> Quaternion Float -> M44 Float
viewMatrix camTranslation camOrientation =
    -- Camera translation is negated since view matrix = inverse of camera matrix
    let mTranslation = identity & translation .~ (-camTranslation)
        mRotation = m33_to_m44 . fromQuaternion $ camOrientation
    in mRotation !*! mTranslation


-- | A behavior describing the camera position
camPosition :: InputEvent
            -> TickEvent
            -> V3 Float
            -> Behavior (Quaternion Float)
            -> MomentIO (Behavior (V3 Float))
camPosition eInput eTick initialPos bCamRot = do
  let speed = 0.1

  left <- keyDown eInput SDL.ScancodeA
  right <- keyDown eInput SDL.ScancodeD
  down <- keyDown eInput SDL.ScancodeS
  up <- keyDown eInput SDL.ScancodeW

  bMBDown <- mouseButtonDown eInput SDL.ButtonLeft

  let eMouseMoved = mouseMoved eInput Absolute

  let bVelocity = (camVelocity speed <$> bCamRot <*> left <*> right <*> down <*> up)

  let bAddVelocity = (+) <$> bVelocity
  let eAddVelocity = bAddVelocity <@ eTick

  bCamPos <- accumB initialPos eAddVelocity

  return bCamPos


-- | Calculates the camera velocity based on speed and inputs
-- The (bool, bool, bool, bool) is (up, down, left, right)

camVelocity :: Float -> Quaternion Float -> Bool
            -> Bool -> Bool -> Bool -> V3 Float
camVelocity speed camRot left right down up =
  let horzVel    True  False = -speed
      horzVel    False True  =  speed
      horzVel    _     _     =  0
      vertVel    True  False =  speed
      vertVel    False True  = -speed
      vertVel    _     _     =  0
  in V3 (horzVel left right)
        (vertVel up down)
        (0.0) *! fromQuaternion camRot


-- | Update camera orientation quaternion based on a mouse movement
-- It basically converts from eulers to axisAngle, and could be abstracted

cameraRotation :: V2 Float -> Quaternion Float
cameraRotation (V2 yaw pitch) = axisAngle (V3 1 0 0) (pitch)
                              * axisAngle (V3 0 1 0) (yaw)
