module Day9
    (
        run
    ) where

import Day5 (runProgram)

run :: IO ()
run = do
    content <- readFile "src/day9_input.txt"
    let program =  head (lines content)
    print ("puzzle 1: " ++ show (runProgram program 1))
    print ("puzzle 2: " ++ show (runProgram program 2))
