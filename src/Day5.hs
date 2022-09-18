module Day5
    (
        runProgram,
        run
    ) where

import Data.Array

import Day2 hiding (run)

runProgram :: String -> Int -> Int
runProgram memory input = input

run :: IO ()
run = do
    content <- readFile "src/day5_input.txt"
    let p1 = runProgram (head (lines content)) 12
    print ("puzzle 1: " ++ show p1)
