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
  )
where

import Framework
import Reactive.Banana
import Reactive.Banana.Frameworks
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
