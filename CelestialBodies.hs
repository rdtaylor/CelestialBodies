module Main where

import System.CPUTime
import Control.Monad
import Graphics.UI.SDL

import Constants
import World
import Graphics
import Entities (Ship, shipForward, shipBack, shipNeutral, Events)
import Events
import Random

main :: IO ()
main = do
  graphics <- initGfx
  let seed = getInitSeed
  mainLoop (getInitWorld seed) 0 delta_time events graphics seed
  where events = getInitEvents

mainLoop :: World -> Time -> Time -> Events -> Graphics -> Integer -> IO ()
mainLoop world prev_update_time delta events graphics seed = do
  curr_time <- getTime
  maybe_events <- handleEvents $ Just events
  let events = case maybe_events of
                 (Just x) -> x

  let (world', is_updated) = (updateLoop world
                                         (curr_time - prev_update_time)
                                         delta
                                         events
                                         seed)
  if is_updated then draw world' prev_update_time graphics
                else return ()

  case maybe_events of
      Nothing -> return ()
      _ -> mainLoop world'
                    (if is_updated then curr_time
                                   else prev_update_time)
                    delta
                    events
                    graphics
                    seed

getTime :: IO Time
getTime = do
  t <- getCPUTime
  return $ (fromIntegral t) / (10^12)

updateLoop :: World -> Time -> Time -> Events -> Seed -> (World, Bool)
updateLoop world elapsed_time delta events seed =
  if elapsed_time < delta
    then (world, False)
    else ((updateWorld world events seed), True)
