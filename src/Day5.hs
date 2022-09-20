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

instance Show State where
  show (State a b c d) = "{" ++ show a ++ "," ++ show b ++  "," ++ show c ++ "," ++ show d ++ "}"

instance Eq State where
  x == y = (memory x == memory y && ip x == ip y && input x == input y && output x == output y)


nextStep::  State ->  State
nextStep state =
    let m = memory state
        i = ip state 
    in case (m ! i) `mod` 10 of
        1  -> process (+)
        2  -> process (*)
        3 -> state { memory = m // [(m ! (i + 1), input state)], ip = i + 2 }
        4 -> state { output = Just (m ! (m ! (i + 1))), ip = i + 2 }
        _ -> state { ip = -i - 1 }
    where
        process op =
            let m = memory state
                i = ip state in
            let a = if (m ! i `div` 100)  `mod` 10 == 1 then m ! (i + 1) else m ! (m ! (i + 1))
                b = if (m ! i `div` 1000) `mod` 10 == 1 then m ! (i + 2) else m ! (m ! (i + 2)) in
                state { memory = (m // [(m ! (i + 3), a `op` b)]), ip = (i + 4)}

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
