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


-- | Focal point of the camera in block space (starting at the
-- bottom left most corner of (0,0))
type InitialPos = V3 Float

-- | Aspect ratio of a window
type Aspect = Float


-- | A mixed-mode perspective/orthographic camera that can transition between the two

mixedCamera :: InputEvent
            -> TickEvent
            -> InitialPos
            -> Aspect
            -> MomentIO (Behavior Camera)
mixedCamera eInput eTick initialPos aspect = do
  -- Perspective fov
  let initialFov = pi / 3

  -- Fixed rotation (an isometric view)
  let initialRot@(V2 initialYaw initialPitch) = V2 (pi/6.0) (-pi/6.0)

  -- Distance at initialFov
  let minDist = 5.0
  let maxDist = 30.0
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
  let clampDist = max minDist . min maxDist
  let updateDist a = clampDist . subtract a
  let calcScreenWidth dist = 2.0 * clampDist dist * tan (0.5 * initialFov)
  let initialScreenWidth = calcScreenWidth initialDist
  bCamDist <- accumB initialDist (updateDist <$> mouseWheelMoved eInput)
  let bScreenWidth = calcScreenWidth <$> bCamDist

  -- The projection matrix
  -- Switch between perspective and ortho when the fov drops to its minimum value
  -- This fixes any visual error that occurs from such a small fov
  let calcOrtho sw = ortho (-sw/2) (sw/2) (-sw*aspect/2) (sw*aspect/2) (-1000) 1000000
  let bPerspective = infinitePerspective <$> bCamFov <*> pure aspect <*> pure 0.1
  let bOrthographic = calcOrtho <$> bScreenWidth
  let bProjection = (\a b c -> if a == minFovValue then b else c)
                          <$> bCamFov <*> bOrthographic <*> bPerspective

  -- Our camera orientation
  -- The orientation is based on the initialRot and given yaw input
  let camRotSpeed = 0.025
  bRotLeft <- keyDown eInput SDL.ScancodeQ
  bRotRight <- keyDown eInput SDL.ScancodeE
  let bRotationVel = (*camRotSpeed) <$> (cameraRotationDir <$> bRotLeft <*> bRotRight)
  bCamYaw <- accumB (initialYaw) (((+) <$> bRotationVel) <@ eTick)
  let bCamPitchYaw = V2 <$> bCamYaw <*> pure initialPitch
  bCamRot <- camOrientation eInput bCamPitchYaw

  -- The position is based on initialPos (the focal point) and the rotation
  -- initialPos isn't actually the camera position but the position it's looking at
  -- bCamPos is actually the camera position for the view matrix
  bCamPos <- positionCamera eInput eTick initialPos bScreenWidth
                            bCamFov bCamPitchYaw bCamRot

  -- Construct view and mvp matrix
  let bViewMatrix = viewMatrix <$> bCamPos <*> bCamRot
  let bMvpMatrix = (!*!) <$> bProjection <*> bViewMatrix

  return $ Camera <$> bProjection <*> bViewMatrix <*> bMvpMatrix
                  <*> bCamPos <*> bCamRot <*> bCamFov


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
               -> Behavior Float
               -> Behavior Float
               -> Behavior (V2 Float)
               -> Behavior (Quaternion Float)
               -> MomentIO (Behavior (V3 Float))
positionCamera eInput eTick initialPos bScreenWidth bFov bCamYawPitch bCamRot = do
  let speed = 0.1

  forwardButton <- keyDown eInput SDL.ScancodeW
  left <- keyDown eInput SDL.ScancodeA
  right <- keyDown eInput SDL.ScancodeD
  down <- keyDown eInput SDL.ScancodeS
  up <- keyDown eInput SDL.ScancodeW

  -- Calculate distance from focal point to achieve desired screen width
  let calcCamDist screenWidth fov = screenWidth / (2.0 * tan (0.5 * fov))
  let bCamDist = calcCamDist <$> bScreenWidth <*> bFov

  -- Forward vector
  let bForwardVec = (!*) <$> (fromQuaternion <$> bCamRot) <*> pure (V3 0 0 (1))

  -- Move the camera back from the block we're looking at by initialDist units
  let bCamOffset = (^*) <$> (bForwardVec) <*> bCamDist

  -- Camera movement
  let bVelocity = (camVelocity speed <$> bCamYawPitch <*> left <*> right
                                     <*> down <*> up)
  let bAddVelocity = (+) <$> bVelocity
  let eAddVelocity = bAddVelocity <@ eTick

  -- The block coordinate the camera is looking at
  bCamLookingAtPos <- accumB initialPos eAddVelocity
  -- Offset the camera back so we're looking at this block
  let bCamPos = (+) <$> bCamLookingAtPos <*> bCamOffset

  return bCamPos


-- | Calculates the camera velocity based on speed and inputs
-- The (bool, bool, bool, bool) is (up, down, left, right)

camVelocity :: Float -> V2 Float -> Bool
            -> Bool -> Bool -> Bool -> V3 Float
camVelocity speed (V2 camYaw _) left right down up =
  let horzVel    True  False = -speed
      horzVel    False True  =  speed
      horzVel    _     _     =  0
      vertVel    True  False = -speed
      vertVel    False True  =  speed
      vertVel    _     _     =  0
      screenVel = V3 (horzVel left right) 0 (vertVel up down)
      -- todo: tie this into the camera rotation so it can be changed at runtime
      rotation = axisAngle (V3 0 1 0) camYaw
  in rotate rotation screenVel


-- | A behavior describing the camera orientation
camOrientation :: InputEvent
               -> Behavior (V2 Float)
               -> MomentIO (Behavior (Quaternion Float))
camOrientation eInput bPitchYaw = do
  bMouseDown <- mouseButtonDown eInput SDL.ButtonLeft

  let eMouseMove = (/300) <$> whenE bMouseDown (mouseMoved eInput Delta)

  bCameraRotationDiff <- accumB (V2 0 0) ((+) <$> eMouseMove)
  let bCameraRotationComponents = (+) <$> bPitchYaw <*> bCameraRotationDiff

  return $ cameraRotation <$> bCameraRotationComponents


-- | Update camera orientation quaternion based on a mouse movement
-- It basically converts from eulers to axisAngle, and could be abstracted

cameraRotation :: V2 Float -> Quaternion Float
cameraRotation (V2 yaw pitch) = axisAngle (V3 0 1 0) (yaw)
                              * axisAngle (V3 1 0 0) (pitch)


-- | Camera rotation direction based on inputs

cameraRotationDir :: Bool -> Bool -> Float
cameraRotationDir True False = -1
cameraRotationDir False True =  1
cameraRotationDir _ _ = 0
