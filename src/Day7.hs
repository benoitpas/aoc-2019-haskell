module Day7
    (
        initAmps, runAmps1, maxSignal1,
        nextCycle, runAmps2, maxSignal2,
        run
    ) where

import Data.Array
import Data.List(permutations, find)
import Data.Maybe(fromJust)

import Day2 (list2array, str2list)

import Day5 (State(..), nextStep, allSteps)

initAmps :: Array Int Int -> [Int] -> [State]
initAmps mem phases =  map (\p -> (fst . nextStep) (State mem 0 p Nothing)) phases 

runAmps1 :: [Int] -> [Int] -> Int
runAmps1 program phases = 
    let mem = list2array program in
    let iStates = initAmps mem phases
    in foldl (\a -> \s -> case output (allSteps s { input = a}) of
                            Just lastOutput -> lastOutput
                            _ -> -1
                            ) 0 iStates

allPhases1 :: [[Int]]
allPhases1 = permutations [0..4]

allPhases2 :: [[Int]]
allPhases2 = permutations [5..9]

maxSignal :: ([Int] -> [Int] -> Int) -> [[Int]] -> [Int] -> Int
maxSignal runAmps allPhases program =
    foldr (\phases -> \maxOutput -> (runAmps program phases) `max` maxOutput) 0 allPhases

maxSignal1 :: [Int] -> Int
maxSignal1 program = maxSignal runAmps1 allPhases1 program

maxSignal2 :: [Int] -> Int
maxSignal2 program = maxSignal runAmps2 allPhases2 program

nextSteps :: State -> (State, Bool)
nextSteps iState = 
    let (nState, cmd) = nextStep iState in
        case cmd of
            4 -> (nState, False)
            99 -> (nState, True)
            _ -> nextSteps nState

nextCycle :: ([State], Int, Bool) -> ([State], Int, Bool)
nextCycle (states, inputSignal, _) = 
    foldl (\(aStates, aSignal, _) -> \s -> 
        let (nState, finished) = nextSteps s  { input = aSignal } in
            (aStates ++ [nState], fromJust (output nState),finished))  ([], inputSignal, False) states

runAmps2 :: [Int] -> [Int] -> Int
runAmps2 program phases = 
    let mem = list2array program in
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
