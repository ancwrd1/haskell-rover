module Lib where

import System.Random (Random (randomR), StdGen)

type Position = (Int, Int)

-- basic math on a pair (Position)
instance (Num a, Num b) => Num (a, b) where
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  negate (x, y) = (-x, -y)
  fromInteger x = (fromIntegral x, fromIntegral x)

-- rover direction
data Direction = North | South | East | West
  deriving (Show, Eq)

-- mission command
data Command = MoveForward | MoveBackward | TurnLeft | TurnRight
  deriving (Show, Eq)

-- terrain with dimensions and list of obstacles
-- coordinate system is:
--   y
--   |
--   |
--   |
--   |___________ x
-- (0,0)
data Terrain = Terrain
  { terrainWidth :: Int,
    terrainHeight :: Int,
    terrainObstacles :: [Position]
  }
  deriving (Eq)

instance Show Terrain where
  show terrain =
    concatMap
      ( \y ->
          map
            (\x -> if isObstacle terrain (x, y) then 'x' else '.')
            [0 .. terrainWidth terrain - 1]
            ++ ['\n']
      )
      [top, top - 1 .. 0]
    where
      top = terrainHeight terrain - 1

-- rover entity with current position and direction
data Rover = Rover
  { roverPosition :: Position,
    roverDirection :: Direction
  }
  deriving (Show, Eq)

-- create new random terrain with a given width, height and rng
newTerrain :: Int -> Int -> StdGen -> Terrain
newTerrain width height gen = Terrain width height $ genObstacles width height gen

-- generate obstacles
genObstacles :: Int -> Int -> StdGen -> [Position]
genObstacles width height gen =
  take (width * height `div` 10)
    . map fst
    . iterate (newPos . snd)
    $ newPos gen
  where
    newPos = randomPosition width height

randomPosition :: Int -> Int -> StdGen -> (Position, StdGen)
randomPosition width height gen = ((randX, randY), gen2)
  where
    (randX, gen1) = randomR (0, width - 1) gen
    (randY, gen2) = randomR (0, height - 1) gen1

wrapPosition :: Terrain -> Position -> Position
wrapPosition terrain position = (newX, newY)
  where
    width = terrainWidth terrain
    height = terrainHeight terrain
    newX
      | fst position < 0 = 0
      | fst position >= width = width - 1
      | otherwise = fst position
    newY
      | snd position < 0 = 0
      | snd position >= height = height - 1
      | otherwise = snd position

isObstacle :: Terrain -> Position -> Bool
isObstacle terrain position = elem position $ terrainObstacles terrain

-- run a single command and return a new rover instance
runCommand :: Terrain -> Rover -> Command -> Rover
runCommand terrain rover MoveForward = Rover (wrapPosition terrain newPos) dir
  where
    dir = roverDirection rover
    pos = roverPosition rover
    newPos = case dir of
      North -> pos + (0, 1)
      South -> pos + (0, -1)
      West -> pos + (-1, 0)
      East -> pos + (1, 0)
runCommand terrain rover MoveBackward = Rover (wrapPosition terrain newPos) dir
  where
    dir = roverDirection rover
    pos = roverPosition rover
    newPos = case dir of
      North -> pos + (0, -1)
      South -> pos + (0, 1)
      West -> pos + (1, 0)
      East -> pos + (-1, 0)
runCommand terrain rover TurnLeft = Rover pos newDir
  where
    dir = roverDirection rover
    pos = roverPosition rover
    newDir = case dir of
      North -> West
      South -> East
      West -> South
      East -> North
runCommand terrain rover TurnRight = Rover pos newDir
  where
    dir = roverDirection rover
    pos = roverPosition rover
    newDir = case dir of
      North -> East
      South -> West
      West -> North
      East -> South

-- run missions
runMission :: Terrain -> Rover -> [Command] -> IO ()
runMission terrain rover [] = putStrLn "Mission successful!"
runMission terrain rover (x : xs) =
  if isObstacle terrain $ roverPosition newRover
    then do
      putStrLn "Mission failed!"
      print newRover
      print terrain
    else runMission terrain newRover xs
  where
    newRover = runCommand terrain rover x
