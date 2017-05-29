{-# LANGUAGE LambdaCase #-}

-- | 
-- Module      :  Game.Simulation.Input
-- Description :  Contains input related functions and signals
--                for the game simulation
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Game.Simulation.Input
  ( keyDown
  , keyPressed
  , keyReleased
  , mouseButtonDown
  , mouseButtonPressed
  , mouseButtonReleased
  , mouseMoved
  , mouseWheelMoved
  , RetType(..)
  )
where

import Framework
import Reactive.Banana
import Reactive.Banana.Frameworks
import Linear.Affine (Point(..))
import Linear.V2
import qualified SDL.Event          as SDL
import qualified SDL.Input.Keyboard as SDL


-- | Predicate to determine whether the given SDL event is a keyboard event

isKeyEvent :: SDL.Event -> Bool
isKeyEvent e = case SDL.eventPayload e of
                    SDL.KeyboardEvent _  -> True
                    _                    -> False


-- | Filters input events down to just keyboard events

keyEvents :: InputEvent -> Event SDL.KeyboardEventData
keyEvents eInput = let keyEvents = filterE isKeyEvent eInput
                       getKeyEventData = (\x ->
                         let SDL.KeyboardEvent d = SDL.eventPayload x in d)
                       eventData = getKeyEventData <$> keyEvents
                   in eventData


-- | Predicate to determine whether a keyboard event is a key change

isKeyChange :: SDL.Scancode -> SDL.InputMotion -> Bool -> SDL.KeyboardEventData -> Bool
isKeyChange expectedScancode expectedMotion allowRepeat keyEventData =
  let SDL.KeyboardEventData _ motion repeat sym = keyEventData
      SDL.Keysym scancode _ _ = sym
  in   motion == expectedMotion
    && allowRepeat == repeat
    && scancode == expectedScancode


-- | An event that fires when a key's state changes

keyChanged :: SDL.InputMotion -> InputEvent -> SDL.Scancode -> Event ()
keyChanged inputMotion eInput scancode =
  let keyEvent = keyEvents eInput
      isKeyPressed = isKeyChange scancode inputMotion False
      scanEvent = filterE isKeyPressed keyEvent
  in () <$ scanEvent


-- | An event that fires when a key is pressed

keyPressed :: InputEvent -> SDL.Scancode -> Event ()
keyPressed = keyChanged SDL.Pressed


-- | An event that fires when a key is released

keyReleased :: InputEvent -> SDL.Scancode -> Event ()
keyReleased = keyChanged SDL.Released


-- | A behavior describing whether a given key is currently held down

keyDown :: InputEvent -> SDL.Scancode -> MomentIO (Behavior Bool)
keyDown eInput scan = let press = True <$ keyPressed eInput scan
                          release = False <$ keyReleased eInput scan
                          keyOnOff = unionWith (\_ _ -> False) press release
                      in stepper False keyOnOff


-- | Predicate to determine whether the given SDL event is a mouse button event

isMouseButtonEvent :: SDL.Event -> Bool
isMouseButtonEvent e = case SDL.eventPayload e of
                    SDL.MouseButtonEvent _  -> True
                    _                       -> False


-- | Filters input events down to just mouse button events

mouseButtonEvents :: InputEvent -> Event SDL.MouseButtonEventData
mouseButtonEvents eInput = let mbEvents = filterE isMouseButtonEvent eInput
                               getMbEventData = (\x ->
                                 let SDL.MouseButtonEvent d = SDL.eventPayload x in d)
                               eventData = getMbEventData <$> mbEvents
                           in eventData


-- | Predicate to determine whether a mouse button event is a button change

isMouseButtonChange :: SDL.MouseButton -> SDL.InputMotion -> SDL.MouseButtonEventData -> Bool
isMouseButtonChange expectedButton expectedMotion mbEventData =
  let SDL.MouseButtonEventData _ motion _ button _ _ = mbEventData
  in   motion == expectedMotion
    && button == expectedButton


-- | An event that fires when a key's state changes

mouseButtonChanged :: SDL.InputMotion -> InputEvent -> SDL.MouseButton -> Event ()
mouseButtonChanged inputMotion eInput button =
  let mbEvent = mouseButtonEvents eInput
      isButtonPressed = isMouseButtonChange button inputMotion
      scanEvent = filterE isButtonPressed mbEvent
  in () <$ scanEvent


-- | An event that fires when a mouse button is pressed

mouseButtonPressed :: InputEvent -> SDL.MouseButton -> Event ()
mouseButtonPressed = mouseButtonChanged SDL.Pressed


-- | An event that fires when a mouse button is released

mouseButtonReleased :: InputEvent -> SDL.MouseButton -> Event ()
mouseButtonReleased = mouseButtonChanged SDL.Released


-- | A behavior describing whether a given mouse button is currently held down

mouseButtonDown :: InputEvent -> SDL.MouseButton -> MomentIO (Behavior Bool)
mouseButtonDown eInput button = let press = True <$ mouseButtonPressed eInput button
                                    release = False <$ mouseButtonReleased eInput button
                                    eButtonDown = unionWith (\_ _ -> False) press release
                                in stepper False eButtonDown


-- | Predicate to determine whether the given SDL event is a mouse motion event

isMouseMotionEvent :: SDL.Event -> Bool
isMouseMotionEvent e = case SDL.eventPayload e of
                    SDL.MouseMotionEvent _  -> True
                    _                       -> False


-- | Filters input events down to just mouse motion events

mouseMotionEvents :: InputEvent -> Event SDL.MouseMotionEventData
mouseMotionEvents eInput = let mbEvents = filterE isMouseMotionEvent eInput
                               getMbEventData = (\x ->
                                 let SDL.MouseMotionEvent d = SDL.eventPayload x in d)
                               eventData = getMbEventData <$> mbEvents
                           in eventData


-- | A sum type for specifying whether the mouse movement event should
-- report a difference or an absolute position

data RetType = Delta | Absolute


-- | Gets either the difference in position or the absolute position of the mouse
-- from an SDL.MouseMotionEventData

mouseMoveValue :: RetType -> SDL.MouseMotionEventData -> V2 Float
mouseMoveValue Delta (SDL.MouseMotionEventData _ _ _ _ (V2 x y)) =
                                       V2 (fromIntegral x) (fromIntegral y)
mouseMoveValue Absolute (SDL.MouseMotionEventData _ _ _ (P (V2 x y)) _) =
                                       V2 (fromIntegral x) (fromIntegral y)


-- | An event that fires when the mouse moves and reports either the
-- difference in position or the new position

mouseMoved :: InputEvent -> RetType -> Event (V2 Float)
mouseMoved eInput retType = let events = mouseMotionEvents eInput
                            in mouseMoveValue retType <$> events


-- | Predicate to determine whether the given SDL event is a mouse wheel event

isMouseWheelEvent :: SDL.Event -> Bool
isMouseWheelEvent e = case SDL.eventPayload e of
                    SDL.MouseWheelEvent _  -> True
                    _                      -> False


-- | Filters input events down to just mouse wheel events

mouseWheelEvents :: InputEvent -> Event SDL.MouseWheelEventData
mouseWheelEvents eInput = let mwEvents = filterE isMouseWheelEvent eInput
                              getMwEventData = (\x ->
                                let SDL.MouseWheelEvent d = SDL.eventPayload x in d)
                              eventData = getMwEventData <$> mwEvents
                          in eventData


-- | Mouse wheel changed value

mouseWheelValue :: SDL.MouseWheelEventData -> Float
mouseWheelValue (SDL.MouseWheelEventData _ _ (V2 _ y) _) = fromIntegral y


-- | An event that fires when the mouse wheel moves

mouseWheelMoved :: InputEvent -> Event Float
mouseWheelMoved eInput = let events = mouseWheelEvents eInput
                         in mouseWheelValue <$> events

