module Lib where

import System.Random

type Position = (Int, Int)

-- basic math on a pair (Position)
instance (Num a, Num b) => Num (a, b) where
    (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
    (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
    abs (x, y) = (abs x, abs y)
    signum (x, y) = (signum x, signum y)
    negate (x, y) = (-x, -y)
    fromInteger x = (fromIntegral x, fromIntegral x)

data Direction = North | South | East | West
    deriving (Show, Eq)

data Command = MoveForward | MoveBackward | TurnLeft | TurnRight
    deriving (Show, Eq)

data Terrain = Terrain {
    terrainWidth :: Int,
    terrainHeight :: Int,
    terrainObstacles :: [Position]
} deriving (Show, Eq)

data Rover = Rover {
    position :: Position,
    direction :: Direction
} deriving (Show, Eq)

newTerrain :: Int -> Int -> StdGen -> Terrain
newTerrain width height gen = Terrain width height $ genObstacles width height gen

genObstacles :: Int -> Int -> StdGen -> [Position]
genObstacles width height gen = take (width * height `div` 10)
    . map fst
    . iterate (newPos . snd) $ newPos gen
    where newPos = randomPosition width height


randomPosition :: Int -> Int -> StdGen -> (Position, StdGen)
randomPosition width height gen = ((randX, randY), gen2)
    where
        (randX, gen1) = randomR (0, width - 1) gen
        (randY, gen2) = randomR (0, height - 1) gen1

wrapPosition :: Terrain -> Position -> Position
wrapPosition terrain position = (newX, newY) where
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

runCommand :: Terrain -> Rover -> Command -> Rover
runCommand terrain rover MoveForward = Rover (wrapPosition terrain newPos) dir where
    dir = direction rover
    pos = position rover
    newPos
        | dir == North = pos + (0, 1)
        | dir == South = pos + (0, -1)
        | dir == West = pos + (-1, 0)
        | dir == East = pos + (1, 0)

runCommand terrain rover MoveBackward = Rover (wrapPosition terrain newPos) dir where
    dir = direction rover
    pos = position rover
    newPos
        | dir == North = pos + (0, -1)
        | dir == South = pos + (0, 1)
        | dir == West = pos + (1, 0)
        | dir == East = pos + (-1, 0)

runCommand terrain rover TurnLeft = Rover pos newDir where
    dir = direction rover
    pos = position rover
    newDir
        | dir == North = West
        | dir == South = East
        | dir == West = South
        | dir == East = North

runCommand terrain rover TurnRight = Rover pos newDir where
    dir = direction rover
    pos = position rover
    newDir
        | dir == North = East
        | dir == South = West
        | dir == West = North
        | dir == East = South

runMission :: Terrain -> Rover -> [Command] -> IO ()
runMission terrain rover commands = print terrain
