module Graphics where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as IMG
import Graphics.UI.SDL.Rotozoomer as Roto

import Constants
import Entities
import World

data Graphics = Graphics {
  screen :: Surface
, background :: Surface
, ship_sprite :: Surface
, bullet_sprite :: Surface
, asteroid_sprite :: Surface
}

initGfx :: IO (Graphics)
initGfx = do
  SDL.init [InitEverything]
  screen <- setVideoMode (fromIntegral window_x) (fromIntegral window_y) 32 [SWSurface]
  setCaption "Celestial Bodies" []

  background <- loadBMP "images/background.bmp"
  ship_sprite <- loadBMP "images/ship.bmp"
  bullet_sprite <- loadBMP "images/bullet.bmp"
  asteroid_sprite <- loadBMP "images/asteroid.bmp"

  return (Graphics {
    screen = screen
  , background = background
  , ship_sprite = ship_sprite
  , bullet_sprite = bullet_sprite
  , asteroid_sprite = asteroid_sprite
  })

draw :: World -> Time -> Graphics -> IO ()
draw world elapsed_time graphics = do
  blitSurface (background graphics) Nothing (screen graphics) Nothing
  mapM_ (\x -> drawAsteroid x graphics) (asteroids world)
  drawShip (ship world) graphics
  mapM_ (\x -> drawBullet x graphics) (bullets world)
  SDL.flip $ screen graphics
  putStrLn $ (show world) ++ " " ++ (show elapsed_time)

drawShip :: Ship -> Graphics -> IO Bool
drawShip ship graphics = do
  rotated_ship <- rotozoom (ship_sprite graphics)
                           ((orientation ship) * (360 / (2 * pi)))
                           1 True
  drawEntity ship rotated_ship (screen graphics)

drawBullet :: Bullet -> Graphics -> IO Bool
drawBullet bullet graphics = do
  drawEntity bullet (bullet_sprite graphics) (screen graphics)

drawAsteroid :: Asteroid -> Graphics -> IO Bool
drawAsteroid asteroid graphics = do
  drawEntity asteroid (asteroid_sprite graphics) (screen graphics)

drawEntity :: Entity a => a -> Surface -> Surface -> IO Bool
drawEntity entity sprite screen = do
  let x_pos = (x . position) entity
  let y_pos = (y. position) entity
  let offset = Just Rect {
    rectX = x_pos - ((surfaceGetWidth sprite) `div` 2)
  , rectY = y_pos - ((surfaceGetHeight sprite) `div` 2)
  , rectW = 0
  , rectH = 0
  }

  blitSurface sprite Nothing screen offset
