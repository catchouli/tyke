{-# LANGUAGE LambdaCase #-}

module Game.Simulation.Camera.MixedCamera
  ( mixedCamera
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


-- | Focal point of the camera in block space (starting at the bottom left most corner of (0,0))
type InitialPos = V3 Float

-- | Aspect ratio of a window
type Aspect = Float


-- | A mixed-mode perspective/orthographic camera that can transition between the two

mixedCamera :: InputEvent
            -> TickEvent
            -> InitialPos
            -> Aspect
            -> MomentIO (Behavior Camera)
mixedCamera eInput eTick initialPos eAspect = do
  -- Perspective fov
  let initialFov = pi / 3

  -- Fixed rotation (an isometric view)
  let initialRot = V2 (pi/6.0) (-pi/6.0)

  -- Distance at initialFov
  let initialDist = 15.0

  -- Camera FOV behavior
  -- Tweens between initialFov and a value close to 0
  -- to give an interpolation between perspective and ortho
  -- the distance also needs to be set correctly for this effect
  -- to work properly
  let minFovValue = 0.02
  let eFovFadeSpeed = 0.1
  let fovFadeFunc = \a -> max minFovValue . min initialFov . (+a)
  let eChangeMode = keyPressed eInput SDL.ScancodeR
  let eChangeDir = pure negate <@ eChangeMode
  bFovFadeDir <- accumB eFovFadeSpeed eChangeDir
  bCamFov <- accumB initialFov ((fovFadeFunc <$> bFovFadeDir) <@ eTick)
  
  -- Calculate desired screen width about focal point from the
  -- initial distance given and fov
  -- The formula for the dolly zoom is distance = width / 2tan(0.5fov)
  -- So we first solve for width using the initial values, and then
  -- solve for distance
  let screenWidth = 2.0 * initialDist * tan (0.5 * initialFov)

  -- The projection matrix
  -- Switch between perspective and ortho when the fov drops to its minimum value
  -- This fixes any visual error that occurs from such a small fov
  let bPerspective = infinitePerspective <$> bCamFov <*> pure eAspect <*> pure 0.1
  let bOrthographic = pure $ ortho (-screenWidth/2) (screenWidth/2)
          (-screenWidth*eAspect/2) (screenWidth*eAspect/2) (-1000.0) 1000.0
  let bProjection = (\a b c -> if a == minFovValue then b else c) <$> bCamFov <*> bOrthographic <*> bPerspective

  -- Our camera orientation signal
  -- The orientation is based on the initialRot and generally doesn't changed
  -- The position is based on initialPos (the focal point) and the rotation
  -- initialPos isn't actually the camera position but the position it's looking at
  -- bCamPos is actually the camera position for the view matrix
  bCamRot <- camOrientation eInput initialRot
  bCamPos <- positionCamera eInput eTick initialPos screenWidth bCamFov bCamRot

  -- Construct view and mvp matrix
  let bViewMatrix = viewMatrix <$> bCamPos <*> bCamRot
  let bMvpMatrix = (!*!) <$> bProjection <*> bViewMatrix

  return $ Camera <$> bProjection <*> bViewMatrix <*> bMvpMatrix <*> bCamPos <*> bCamRot <*> bCamFov


-- | Convert a translation matrix and a quaternion to a view matrix

viewMatrix :: V3 Float -> Quaternion Float -> M44 Float
viewMatrix camTranslation camOrientation =
    -- Camera translation/rotation is negated since view matrix = inverse
    -- of camera matrix
    let mTranslation = identity & translation .~ (-camTranslation)
        mRotation = m33_to_m44 . fromQuaternion $ camOrientation
    in (inv44 mRotation) !*! mTranslation


-- | A behavior describing the camera position
positionCamera :: InputEvent
               -> TickEvent
               -> V3 Float
               -> Float
               -> Behavior Float
               -> Behavior (Quaternion Float)
               -> MomentIO (Behavior (V3 Float))
positionCamera eInput eTick initialPos screenWidth bFov bCamRot = do
  let speed = 0.1

  forwardButton <- keyDown eInput SDL.ScancodeW
  left <- keyDown eInput SDL.ScancodeA
  right <- keyDown eInput SDL.ScancodeD
  down <- keyDown eInput SDL.ScancodeS
  up <- keyDown eInput SDL.ScancodeW

  -- Calculate distance from focal point to achieve desired screen width
  let bCamDist = (\fov -> screenWidth / (2.0 * tan(0.5 * fov))) <$> bFov

  -- Forward vector
  let bForwardVec = (!*) <$> (fromQuaternion <$> bCamRot) <*> pure (V3 0 0 (1))

  -- Move the camera back from the block we're looking at by initialDist units
  let bCamOffset = (^*) <$> (bForwardVec) <*> bCamDist

  let eMouseMoved = mouseMoved eInput Absolute

  let bVelocity = (camVelocity speed <$> bCamRot <*> left <*> right
                                     <*> down <*> up)

  let bAddVelocity = (+) <$> bVelocity
  let eAddVelocity = bAddVelocity <@ eTick

  bCamLookingAtPos <- accumB initialPos eAddVelocity
  let bCamPos = (+) <$> bCamLookingAtPos <*> bCamOffset

  return bCamPos


-- | Calculates the camera velocity based on speed and inputs
-- The (bool, bool, bool, bool) is (up, down, left, right)

camVelocity :: Float -> Quaternion Float -> Bool
            -> Bool -> Bool -> Bool -> V3 Float
camVelocity speed camRot left right down up =
  let horzVel    True  False = -speed
      horzVel    False True  =  speed
      horzVel    _     _     =  0
      vertVel    True  False = -speed
      vertVel    False True  =  speed
      vertVel    _     _     =  0
  in V3 (horzVel left right)
        (0)
        (vertVel up down)


-- | A behavior describing the camera orientation
camOrientation :: InputEvent -> V2 Float -> MomentIO (Behavior (Quaternion Float))
camOrientation eInput initialRot = do
  bMouseDown <- mouseButtonDown eInput SDL.ButtonLeft

  let eMouseMove = (/300) <$> whenE bMouseDown (mouseMoved eInput Delta)

  bCameraRotationComponents <- accumB initialRot ((+) <$> eMouseMove)

  return $ cameraRotation <$> bCameraRotationComponents


-- | Update camera orientation quaternion based on a mouse movement
-- It basically converts from eulers to axisAngle, and could be abstracted

cameraRotation :: V2 Float -> Quaternion Float
cameraRotation (V2 yaw pitch) = axisAngle (V3 0 1 0) (yaw)
                              * axisAngle (V3 1 0 0) (pitch)


-- | The change in fov per second according to whether the buttons are down

fovChange :: Bool -> Bool -> Float
fovChange False True =  0.5
fovChange True False = -0.5
fovChange _ _ = 0.0
