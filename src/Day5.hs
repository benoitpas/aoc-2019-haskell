module Day5
    (
        State (..), stateFromMemory, stateFromProgram,
        nextStep, allSteps,
        runProgram,
        run
    ) where

import Data.Map

import Day2 (str2memory, Memory)

data State = State {
    memory :: Memory,
    ip :: Integer,
    base :: Integer,
    input :: Integer,
    output:: [Integer]
}

stateFromMemory :: Memory -> Integer -> State
stateFromMemory m i = State m 0 0 i []

stateFromProgram :: String -> Integer -> State
stateFromProgram program i = stateFromMemory (str2memory program) i

instance Show State where
  show (State a b c d e) = "{" ++ show a ++ "," ++ show b ++  "," ++ show c ++ "," ++ show d ++ "," ++ show e ++ "}"

instance Eq State where
  x == y = (memory x == memory y && ip x == ip y && input x == input y && output x == output y)


nextStep::  State -> (State, Integer)
nextStep state =
    let m = memory state
        i = ip state in
    let readM idx = findWithDefault 0 idx m in
    let mode inc = (readM i `div` 10 ^ (inc + 1)) `mod` 10 in
    let param inc = case mode inc of
                        0 -> readM (readM (i + inc))
                        1 -> readM (i + inc)
                        2 -> readM (readM (i + inc) + base state)
        writeIdx inc = readM (i + inc) + if (mode inc) == 2 then base state else 0 in
    let process op = state { memory = insert (writeIdx 3) (param 1 `op` param 2) m, ip = i + 4}
        comp    op = state { memory = insert (writeIdx 3) (if (param 1) `op` (param 2) then 1 else 0) m, ip = i + 4}
        cmd = (m ! i) `mod` 100 in
    let newState = case cmd of
            1  -> process (+)
            2  -> process (*)
            3 -> state { memory = insert (writeIdx 1) (input state) m, ip = i + 2 }
            4 -> state { output = output state ++ [param 1] , ip = i + 2 }
            5 -> state { ip = if (param 1) > 0 then param 2 else i + 3 }
            6 -> state { ip = if (param 1) == 0 then param 2 else i + 3 }
            7 -> comp (<)
            8 -> comp (==)
            9 -> state { base = base state + param 1, ip = i + 2 }
            _ -> state { ip = -i - 1 }
    in (newState, cmd)
               
allSteps :: State -> State
allSteps iState = until (\state -> ip state < 0) (fst . nextStep) iState

runProgram :: String -> Integer -> Integer
runProgram program i =
    let iState = stateFromProgram program i in
    let lastState = allSteps iState
    in last $ output lastState

run :: IO ()
run = do
    content <- readFile "src/day5_input.txt"
    let program = (head (lines content))
    let p1 = runProgram program 1
    print ("puzzle 1: " ++ show p1)
    let p2 = runProgram program 5
    print ("puzzle 2: " ++ show p2)
