module Day5
    (
        State (..), nextStep, allSteps,
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

instance Show State where
  show (State a b c d) = "{" ++ show a ++ "," ++ show b ++  "," ++ show c ++ "," ++ show d ++ "}"

instance Eq State where
  x == y = (memory x == memory y && ip x == ip y && input x == input y && output x == output y)


nextStep::  State ->  State
nextStep state =
    let m = memory state
        i = ip state in
        let param ip inc = if (m ! ip `div` (if inc == 1 then 100 else 1000) `mod` 10) == 1 then m ! (ip + inc) else m ! (m ! (ip + inc))
            process op = state { memory = (m // [(m ! (i + 3), (param i 1) `op` (param i 2))]), ip = (i + 4)}
            compare op = state { memory = m // [(m ! (i + 3), if (param i 1) `op` (param i 2) then 1 else 0)], ip = (i + 4)}
        in case (m ! i) `mod` 10 of
            1  -> process (+)
            2  -> process (*)
            3 -> state { memory = m // [(m ! (i + 1), input state)], ip = i + 2 }
            4 -> state { output = Just (param i 1), ip = i + 2 }
            5 -> state { ip = if (param i 1) > 0 then param i 2 else i + 3 }
            6 -> state { ip = if (param i 1) == 0 then param i 2 else i + 3 }
            7 -> compare (<)
            8 -> compare (==)
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
    let program = (head (lines content))
    let p1 = runProgram program 12
    print ("puzzle 1: " ++ show p1)
    let p2 = runProgram program 5
    print ("puzzle 2: " ++ show p2)
