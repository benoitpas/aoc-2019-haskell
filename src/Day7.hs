module Day7
    (
        initAmps, runAmps1, maxSignal1,
        nextCycle, runAmps2, maxSignal2,
        run
    ) where

import Data.List(permutations, find)

import Day2 (list2memory, str2list, Memory)

import Day5 (State(..), stateFromMemory, nextStep, allSteps)

initAmps :: Memory -> [Integer] -> [State]
initAmps mem phases =  map (\p -> (fst . nextStep) (stateFromMemory mem p)) phases 

runAmps1 :: [Integer] -> [Integer] -> Integer
runAmps1 program phases = 
    let mem = list2memory program in
    let iStates = initAmps mem phases
    in foldl (\a -> \s -> case output (allSteps s { input = a}) of
                            [] -> -1
                            l -> last l
                            ) 0 iStates

allPhases1 :: [[Integer]]
allPhases1 = permutations [0..4]

allPhases2 :: [[Integer]]
allPhases2 = permutations [5..9]

maxSignal :: ([Integer] -> [Integer] -> Integer) -> [[Integer]] -> [Integer] -> Integer
maxSignal runAmps allPhases program =
    foldr (\phases -> \maxOutput -> (runAmps program phases) `max` maxOutput) 0 allPhases

maxSignal1 :: [Integer] -> Integer
maxSignal1 program = maxSignal runAmps1 allPhases1 program

maxSignal2 :: [Integer] -> Integer
maxSignal2 program = maxSignal runAmps2 allPhases2 program

nextSteps :: State -> (State, Bool)
nextSteps iState = 
    let (nState, cmd) = nextStep iState in
        case cmd of
            4 -> (nState, False)
            99 -> (nState, True)
            _ -> nextSteps nState

nextCycle :: ([State], Integer, Bool) -> ([State], Integer, Bool)
nextCycle (states, inputSignal, _) = 
    foldl (\(aStates, aSignal, _) -> \s -> 
        let (nState, finished) = nextSteps s  { input = aSignal } in
            (aStates ++ [nState], last (output nState),finished))  ([], inputSignal, False) states

runAmps2 :: [Integer] -> [Integer] -> Integer
runAmps2 program phases = 
    let mem = list2memory program in
    let iStates = initAmps mem phases in
    let cycles = iterate nextCycle (iStates, 0, False) in
        case find (\(_,_,f) -> f) cycles of
            Just (_, outputSignal, _) -> outputSignal
            _ -> -1
 
run :: IO ()
run = do
    content <- readFile "src/day7_input.txt"
    let program = str2list $ head (lines content)
    print ("puzzle 1: " ++ show (maxSignal1 program))
    print ("puzzle 2: " ++ show (maxSignal2 program))
