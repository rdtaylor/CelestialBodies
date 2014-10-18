module Events where

import Graphics.UI.SDL as SDL

import Entities

getInitEvents :: Events
getInitEvents = Events {
  thrust = shipNeutral
, turn = shipNeutralTurn
, shoot = False
}

handleEvents :: Maybe Events -> IO (Maybe Events)
handleEvents maybe_events = do
  let events = case maybe_events of
               (Just x) -> x

  event <- pollEvent
  case event of
    Quit    -> return Nothing
    NoEvent -> return $ maybe_events
    (KeyDown (Keysym SDLK_UP _ _)) ->
      return $ Just $ events { thrust = shipForward }
    (KeyUp (Keysym SDLK_UP _ _)) ->
      return $ Just $ events { thrust = shipNeutral }
    (KeyDown (Keysym SDLK_DOWN _ _)) ->
      return $ Just $ events { thrust = shipBack }
    (KeyUp (Keysym SDLK_DOWN _ _)) ->
      return $ Just $ events { thrust = shipNeutral }
    (KeyDown (Keysym SDLK_LEFT _ _)) ->
      return $ Just $ events { turn = shipLeft }
    (KeyUp (Keysym SDLK_LEFT _ _)) ->
      return $ Just $ events { turn = shipNeutralTurn }
    (KeyDown (Keysym SDLK_RIGHT _ _)) ->
      return $ Just $ events { turn = shipRight }
    (KeyUp (Keysym SDLK_RIGHT _ _)) ->
      return $ Just $ events { turn = shipNeutralTurn }
    (KeyDown (Keysym SDLK_SPACE _ _)) ->
      return $ Just $ events { shoot = True }
    (KeyUp (Keysym SDLK_SPACE _ _)) ->
      return $ Just $ events { shoot = False }
    (KeyDown (Keysym SDLK_ESCAPE _ _)) -> return Nothing
    _ -> handleEvents maybe_events
