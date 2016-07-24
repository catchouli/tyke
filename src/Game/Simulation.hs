-- | 
-- Module      :  Game.Simulation
-- Description :  The main entry into the game's simulation
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Game.Simulation
  ( gameNetwork
  )
where

import Linear
import Framework
import Game.Data
import Game.Simulation.Input
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified SDL.Event          as SDL
import qualified SDL.Input.Keyboard as SDL


-- | The overall game behavior, which takes input and tick events
-- and produces a Game
gameNetwork :: InputEvent -> TickEvent -> MomentIO (Behavior Game)
gameNetwork eInput eTick = do
  -- Current time as an event
  eTime <- accumE 0 ((+) <$> eTick)

  -- Current time as a behavior
  bTime <- stepper 0 eTime

  -- Camera position and orientation
  bCamRot <- camOrientation eInput
  bCamPos <- camPosition eInput eTick bCamRot

  -- The overall game behavior
  return $ Game <$> bTime <*> bCamPos <*> bCamRot


-- | A behavior describing the camera position
camPosition :: InputEvent
            -> TickEvent
            -> Behavior (Quaternion Float)
            -> MomentIO (Behavior (V3 Float))
camPosition eInput eTick bCamRot = do
  let speed = 0.1
  let initialPos = (V3 20 20 100 :: V3 Float)

  bWDown <- keyDown eInput SDL.ScancodeW
  bSDown <- keyDown eInput SDL.ScancodeS
  bADown <- keyDown eInput SDL.ScancodeA
  bDDown <- keyDown eInput SDL.ScancodeD
  bQDown <- keyDown eInput SDL.ScancodeQ
  bEDown <- keyDown eInput SDL.ScancodeE

  bMBDown <- mouseButtonDown eInput SDL.ButtonLeft

  let eMouseMoved = mouseMoved eInput Absolute

  let bVelocity = (camVelocity speed <$> bCamRot <*> bWDown
                                     <*> bSDown  <*> bADown <*> bDDown
                                     <*> bQDown  <*> bEDown)

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
camOrientation :: InputEvent -> MomentIO (Behavior (Quaternion Float))
camOrientation eInput = do
  bMouseDown <- mouseButtonDown eInput SDL.ButtonLeft

  let eMouseMove = whenE bMouseDown (mouseMoved eInput Delta)

  bCameraRotationComponents <- accumB (V2 0 0 :: V2 Float) ((+) <$> eMouseMove)

  return $ cameraRotation <$> bCameraRotationComponents


-- | Update camera orientation quaternion based on a mouse movement
-- It basically converts from eulers to axisAngle, and could be abstracted

cameraRotation :: V2 Float -> Quaternion Float
cameraRotation (V2 yaw pitch) = axisAngle (V3 1 0 0) (pitch / 300)
                              * axisAngle (V3 0 1 0) (yaw / 300)
