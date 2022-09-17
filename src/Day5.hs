module Day5
    (
        run
    ) where

import Data.Array

import Day2 hiding (run)

runProgram :: (Array Int Int) -> Int -> Int
runProgram memory input = input

run :: IO ()
run = do
    content <- readFile "src/day5_input.txt"
    let memory = Day2.str2array (head (lines content))
    let p1 = runProgram memory 12
    print ("puzzle 1: " ++ show p1)
