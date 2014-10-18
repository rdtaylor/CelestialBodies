module World (
  World
, ship
, bullets
, asteroids
, GameEventType
, getInitWorld
, updateWorld
) where

import GHC.Float (double2Int)

import Constants
import Entities
import Random


data World = World {
  ship :: Ship
, bullets :: [Bullet]
, asteroids :: [Asteroid]
} deriving (Show)

data GameEventType = AsteroidShot | ShipHit deriving (Show, Enum, Eq)

data GameEvent = GameEvent {
  gameEvent_position :: Position
, gameEvent_type :: GameEventType
, gameEvent_extra :: Int
} deriving (Show)


getInitWorld :: Integer -> World
getInitWorld seed = World {
  ship = getInitShip
, bullets = []
, asteroids = getInitAsteroids 5 seed
}

updateWorld :: World -> Events -> Seed -> World
updateWorld world events seed = handleCollisions world'' game_events seed
  where world' = updateEntityPositions world events
        (world'', game_events) = filterCollisions world'

updateEntityPositions :: World -> Events -> World
updateEntityPositions world events = World {
  ship = ship'
, bullets = bullets'
, asteroids = asteroids'
} where ship' = (thrust events) . (turn events) $ ship world
        updatedBullets = updateBullets $ bullets world
        bullets' = if (shoot events)
          then [createBullet (getBulletInitPos ship')
                             (orientation ship')] ++
               updatedBullets
          else updatedBullets
        asteroids' = updateAsteroids (asteroids world)

filterCollisions :: World -> (World, [GameEvent])
filterCollisions world =
  (world { bullets = bullets', asteroids = asteroids' }, game_events)
  where (bullets', asteroids', game_events) =
          (filterCollisionsBullets (bullets world)
                                   (asteroids world)
                                   [])

filterCollisionsBullets :: [Bullet] -> [Asteroid] -> [GameEvent] ->
                           ([Bullet], [Asteroid], [GameEvent])
filterCollisionsBullets [] asteroids game_events = ([], asteroids, [])
filterCollisionsBullets (bullet:bullets) asteroids game_events =
  mergeCollisionTuples (bullet', asteroids', game_events')
                       (filterCollisionsBullets bullets asteroids' [])
  where (bullet', asteroids', game_events') =
          (filterCollisionsBullet bullet asteroids)

mergeCollisionTuples :: ([Bullet], [Asteroid], [GameEvent]) ->
                        ([Bullet], [Asteroid], [GameEvent]) ->
                        ([Bullet], [Asteroid], [GameEvent])
mergeCollisionTuples (b1, a1, ge1) (b2, a2, ge2) =
  (b1 ++ b2, a2, ge1 ++ ge2)

filterCollisionsBullet :: Bullet -> [Asteroid] ->
                          ([Bullet], [Asteroid], [GameEvent])
filterCollisionsBullet bullet [] = ([bullet], [], [])
filterCollisionsBullet bullet (asteroid:asteroids) =
  if has_collision
      then ([], asteroids, [new_game_event])
      else concatTupleLists ([], [asteroid], [])
                            (filterCollisionsBullet bullet
                                                    asteroids)
  where has_collision = (distance_between <= hit_dist_threshold)
        distance_between = sqrt $ (bx - ax)^2 + (by - ay)^2
        bx = (fromIntegral $ (x . position) bullet) :: Double
        by = (fromIntegral $ (y . position) bullet) :: Double
        ax = (fromIntegral $ (x . position) asteroid) :: Double
        ay = (fromIntegral $ (y . position) asteroid) :: Double
        new_game_event = (GameEvent (Position (double2Int ax) (double2Int ay))
                                    AsteroidShot
                                    (case (ast_size asteroid) of
                                        AsteroidSize x -> x))

concatTupleLists :: ([Bullet], [Asteroid], [GameEvent]) ->
                    ([Bullet], [Asteroid], [GameEvent]) ->
                    ([Bullet], [Asteroid], [GameEvent])
concatTupleLists (b1, a1, ge1) (b2, a2, ge2) = (b1 ++ b2, a1 ++ a2, ge1 ++ ge2)

handleCollisions :: World -> [GameEvent] -> Seed -> World
handleCollisions world [] seed = world
handleCollisions world (game_event:game_events) seed
  | AsteroidShot == (gameEvent_type game_event) =
      (handleCollisions world {
          asteroids = new_asteroid1 ++ new_asteroid2 ++ (asteroids world)
      } game_events seed'')
      where (new_asteroid1, seed') = createPositionAsteroid ast_pos
                                                            ast_size
                                                            seed
            (new_asteroid2, seed'') = createPositionAsteroid ast_pos
                                                             ast_size
                                                             seed'
            ast_pos = gameEvent_position game_event
            ast_size = (gameEvent_extra game_event) - 1
