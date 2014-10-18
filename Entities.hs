module Entities where

import GHC.Float (double2Int)

import Constants
import Random


type Orientation = Double
type Velocity = Double

data Position = Position {
    x :: Int 
    , y :: Int 
} deriving (Show)

data EntityVector = EntityVector {
    delta_x :: Velocity
    , delta_y :: Velocity
} deriving (Show)

data Events = Events {
    thrust :: Ship -> Ship
    , turn :: Ship -> Ship
    , shoot :: Bool
}

initial_x = window_x `div` 2 :: Int
initial_y = window_y `div` 2 :: Int
full_circle = 2 * pi

ship_accel = 0.75 :: Velocity
ship_max_speed = 10
ship_turn_speed = pi * delta_time :: Orientation -- Half a rotation per second
ship_bullet_adj = 12 :: Orientation

bullet_speed = 15
bullet_max_lifetime = 1.00

asteroid_max_size = 5
asteroid_max_velocity = 5

class Entity a where
  position :: a -> Position
  orientation :: a -> Orientation
  velocity :: a -> EntityVector
  updateEntity :: a -> Orientation -> EntityVector -> Bool -> a
    
  updatePosition :: a -> Position
  updatePosition entity =
    let x' = ((fromIntegral $ (x . position) entity) :: Velocity) +
             ((delta_x . velocity) entity)
        y' = ((fromIntegral $ (y . position) entity) :: Velocity) +
             ((delta_y . velocity) entity)
    in Position {
      x = constrainX x'
    , y = constrainY y'
    }
    
  updateOrientation :: a -> Orientation -> Orientation
  updateOrientation entity delta_orient =
    let o' = (orientation entity) + delta_orient
    in constrainOrientation o'
    
  updateVelocity :: a -> EntityVector -> EntityVector
  updateVelocity ship delta_velocity =
    let delta_x' = ((delta_x . velocity) ship) + (delta_x delta_velocity)
        delta_y' = ((delta_y . velocity) ship) + (delta_y delta_velocity)
    in constrainV delta_x' delta_y'


data Ship = Ship {
  ship_position :: Position
, ship_orientation :: Orientation
, ship_velocity :: EntityVector
, ship_lives :: Int
} deriving (Show)

instance Entity Ship where
  position (Ship { ship_position=position } ) = position
  orientation (Ship { ship_orientation=orientation } ) = orientation
  velocity (Ship { ship_velocity=velocity } ) = velocity

  updateEntity ship delta_orient delta_velocity moving = Ship {
    ship_position = if moving then updatePosition ship else (position ship)
  , ship_orientation = updateOrientation ship delta_orient
  , ship_velocity = if moving then updateVelocity ship delta_velocity
                              else (velocity ship)
  , ship_lives = (ship_lives ship)
  }


data Bullet = Bullet {
  bullet_position :: Position
, bullet_orientation :: Orientation
, bullet_velocity :: EntityVector
, bullet_lifetime :: Time
} deriving (Show)

instance Entity Bullet where
  position (Bullet { bullet_position=position } ) = position
  orientation (Bullet { bullet_orientation=orientation } ) = orientation
  velocity (Bullet { bullet_velocity=velocity } ) = velocity

  updateEntity bullet delta_orient delta_velocity moving = 
    bullet {
      bullet_position = updatePosition bullet
    , bullet_lifetime = (bullet_lifetime) bullet - delta_time
    }


data AsteroidSize = AsteroidSize Int deriving (Show)
asteroidSize :: Int -> AsteroidSize
asteroidSize n
  | (n < 1) || (n > asteroid_max_size) = error $ "Invalid asteroid size: " ++
                                                 (show n)
  | otherwise = AsteroidSize n

data Asteroid = Asteroid {
  ast_position :: Position
, ast_orientation :: Orientation
, ast_velocity :: EntityVector
, ast_size :: AsteroidSize
} deriving (Show)

instance Entity Asteroid where
  position (Asteroid { ast_position=position } ) = position
  orientation (Asteroid { ast_orientation=orientation } ) = orientation
  velocity (Asteroid { ast_velocity=velocity } ) = velocity

  updateEntity asteroid delta_orient delta_velocity moving = 
    asteroid {
      ast_position = updatePosition asteroid
    }


getInitShip :: Ship
getInitShip = Ship {
  ship_position = Position initial_x initial_y
, ship_orientation = pi / 2 -- North
, ship_velocity = EntityVector 0 0
, ship_lives = 3
}

shipForward :: Ship -> Ship
shipForward ship = updateEntity ship 0.0 (EntityVector delta_x' delta_y') True
  where delta_x' = ship_accel * (cos $ orientation ship)
        delta_y' = ship_accel * (-(sin $ orientation ship))

shipBack :: Ship -> Ship
shipBack ship = updateEntity ship 0.0 (EntityVector delta_x' delta_y') True
  where delta_x' = -ship_accel * (cos $ orientation ship)
        delta_y' = -ship_accel * (-(sin $ orientation ship))

shipNeutral :: Ship -> Ship
shipNeutral ship = updateEntity ship 0 (EntityVector 0 0) True

shipLeft :: Ship -> Ship
shipLeft ship = updateEntity ship ship_turn_speed (EntityVector 0 0) False

shipRight :: Ship -> Ship
shipRight ship = updateEntity ship (-ship_turn_speed) (EntityVector 0 0) False

shipNeutralTurn :: Ship -> Ship
shipNeutralTurn ship = ship

createBullet :: Position -> Orientation -> Bullet
createBullet position orientation = Bullet {
  bullet_position=position
, bullet_orientation=orientation
, bullet_velocity=(EntityVector ((cos orientation) * bullet_speed)
                                (-((sin orientation) * bullet_speed)))
, bullet_lifetime=bullet_max_lifetime
}

updateBullets :: [Bullet] -> [Bullet]
updateBullets bullets = map updateBullet $ filter filterOldBullets bullets

filterOldBullets :: Bullet -> Bool
filterOldBullets bullet = if ((bullet_lifetime bullet) > 0)
  then True
  else False

updateBullet :: Bullet -> Bullet
updateBullet bullet = updateEntity bullet 0 (EntityVector 0 0) True

getBulletInitPos :: Ship -> Position
getBulletInitPos ship = Position {
  x = double2Int $ x_pos + (ship_bullet_adj * (cos curr_orientation))
, y = double2Int $ y_pos - (ship_bullet_adj * (sin curr_orientation))
} where x_pos = (fromIntegral $ (x . position) ship)
        y_pos = (fromIntegral $ (y . position) ship)
        curr_orientation = (orientation ship)

getInitAsteroids :: Int -> Seed -> [Asteroid]
getInitAsteroids 0 _ = []
getInitAsteroids n_asteroids seed =
  new_asteroid ++ (getInitAsteroids (n_asteroids - 1) seed')
  where (new_asteroid, seed') = createRandomAsteroid asteroid_max_size seed

createAsteroid :: Position -> EntityVector -> Int -> [Asteroid]
createAsteroid _ _ 0 = []
createAsteroid position velocity asteroid_size = [Asteroid {
  ast_position = position
, ast_orientation = 0
, ast_velocity = velocity
, ast_size = asteroidSize asteroid_size
}]

createPositionAsteroid :: Position -> Int -> Seed -> ([Asteroid], Seed)
createPositionAsteroid position asteroid_size seed =
  (createAsteroid position
                  (EntityVector (fromIntegral v_x) (fromIntegral v_y))
                  asteroid_size,
                  seed'')
  where RandomWithSeed {value=v_x, seed=seed'} =
          getRandom seed ((-asteroid_max_velocity),
                          asteroid_max_velocity)
        RandomWithSeed {value=v_y, seed=seed''} =
          getRandom seed' ((-asteroid_max_velocity),
                           asteroid_max_velocity)

createRandomAsteroid :: Int -> Seed -> ([Asteroid], Seed)
createRandomAsteroid asteroid_size seed =
  (createPositionAsteroid (Position (fromIntegral ast_x) (fromIntegral ast_y))
                          asteroid_size
                          seed'')
  where RandomWithSeed {value=ast_x, seed=seed'} =
          getRandom seed (0, (fromIntegral window_x) :: Integer)
        RandomWithSeed {value=ast_y, seed=seed''} =
          getRandom seed' (0, (fromIntegral window_y) :: Integer)

updateAsteroids :: [Asteroid] -> [Asteroid]
updateAsteroids asteroids =
  map updateAsteroid $ filter filterDeadAsteroids asteroids

filterDeadAsteroids :: Asteroid -> Bool
filterDeadAsteroids asteroid = True

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid asteroid = updateEntity asteroid 0 (EntityVector 0 0) True

constrainX :: Velocity -> Int
constrainX x'
  | x' < 0 = double2Int $ x' + ((fromIntegral window_x) :: Velocity)
  | x' > ((fromIntegral window_x) :: Velocity) =
    double2Int $ x' - ((fromIntegral window_x) :: Velocity)
  | otherwise = double2Int x'

constrainY :: Velocity -> Int
constrainY y'
  | y' < 0 = double2Int $ y' + ((fromIntegral window_y) :: Velocity)
  | y' > ((fromIntegral window_y) :: Velocity) =
    double2Int $ y' - ((fromIntegral window_y) :: Velocity)
  | otherwise = double2Int $ y'

constrainOrientation :: Orientation -> Orientation
constrainOrientation o
  | o < 0           = o + full_circle
  | o > full_circle = o - full_circle
  | otherwise       = o

constrainV :: Velocity -> Velocity -> EntityVector
constrainV delta_x' delta_y' = if magnitude > ship_max_speed
  then EntityVector {
    delta_x=delta_x' * normalizing_ratio
  , delta_y=delta_y' * normalizing_ratio
  } 
  else EntityVector { delta_x=delta_x', delta_y=delta_y' }
  where magnitude = sqrt $ delta_x'^2 + delta_y'^2
        normalizing_ratio = (ship_max_speed / magnitude)
