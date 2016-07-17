module GameLoop
  ( gameLoop
  )
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified SDL.Event                       as SDL
import qualified Graphics.Gloss                  as Gloss
import qualified Graphics.Gloss.Rendering        as Gloss


type InputHandler  = SDL.Event -> IO ()
type UpdateHandler = () -> IO ()
type RenderHandler = () -> IO ()

type GameNetworkDescription = Gloss.State -> Event (SDL.Event) -> Event () -> MomentIO (Behavior (IO ()))


-- Initialise the game, generating actions for input handling, updating, and IO
gameLoop :: GameNetworkDescription -> IO (InputHandler, UpdateHandler, RenderHandler)
gameLoop gameNetwork = do
  -- Create input, update, and render handlers
  (ahInput,  inputHandler)  <- newAddHandler
  (ahUpdate, updateHandler) <- newAddHandler
  (ahRender, renderHandler) <- newAddHandler

  glossState <- Gloss.initState

  network <- compile $ do
    eInput <- fromAddHandler ahInput
    eUpdate <- fromAddHandler ahUpdate
    eRender <- fromAddHandler ahRender

    bGame <- gameNetwork glossState eInput eUpdate

    reactimate $ bGame <@ eRender

  actuate network

  -- Return handlers
  return (inputHandler, updateHandler, renderHandler)
