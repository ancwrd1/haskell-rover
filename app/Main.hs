module Main where

import Lib
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    let width = 10
    let height = 10
    let position = fst $ randomPosition width height gen
    let terrain = newTerrain width height gen
    let rover = Rover position North
    let commands = [MoveForward, MoveForward, TurnLeft, MoveForward, TurnLeft, MoveForward]
    putStr "Initial position: "
    print rover
    runMission terrain rover commands
