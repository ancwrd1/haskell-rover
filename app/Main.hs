module Main where

import Lib
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    let terrain = newTerrain 10 10 gen
    let rover = Rover (3, 3) North
    runMission terrain rover [MoveForward, MoveForward, TurnLeft, MoveForward, TurnLeft, MoveBackward]
