module Game.Simulation.Camera.IsometricCamera
  ( isometricCamera
  )
where

import Linear
import Framework
import Reactive.Banana
import Reactive.Banana.Frameworks

isometricCamera :: InputEvent
                -> TickEvent
                -> MomentIO (Behavior (M44 Float))
isometricCamera eInput eTick = do
  let dist = realToFrac $ sqrt (1/3)
  let camPos = V3 dist dist dist
  let lookAtPos = V3 0 0 0
  let upVec = V3 0 1 0
  let projection = lookAt camPos lookAtPos upVec
  return . pure $ projection
