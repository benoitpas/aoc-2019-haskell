module Day5
    (
        State (..), nextStep,
        runProgram,
        run
    ) where

import Data.Array

import Day2  (str2array)

data State = State {
    memory :: Array Int Int,
    ip :: Int,
    input :: Int,
    output:: Maybe Int
}

nextStep::  State ->  State
nextStep state =
    let m = memory state
        i = ip state 
    in case m ! ip state of
        3 -> state { memory = m // [(m ! (i + 1), input state)], ip = i + 2 }
        4 -> state { output = Just (m ! (m ! (i + 1))), ip = i + 2 }
        _ -> state { ip = -i - 1 }

allSteps :: State -> State
allSteps state = last $ takeWhile (\state -> ip state >= 0) (iterate nextStep state)

runProgram :: String -> Int -> Maybe Int
runProgram program i =
    let iState = State { memory = str2array program, ip = 0, input = i, output = Nothing }
    in
        let lastState = allSteps iState
        in output lastState

run :: IO ()
run = do
    content <- readFile "src/day5_input.txt"
    let p1 = runProgram (head (lines content)) 12
    print ("puzzle 1: " ++ show p1)
