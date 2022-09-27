module Day7
    (
        runAmps, maxSignal,
        run
    ) where

import Data.List(permutations)

import Day2 (list2array, str2list)

import Day5 (State(..), nextStep, allSteps)

runAmps :: [Int] -> [Int] -> Int
runAmps program phases = 
    let mem = list2array program in
    let iStates = map (\p -> nextStep (State mem 0 p Nothing)) phases 
    in foldl (\a -> \s -> case output (allSteps s { input = a}) of
                            Just lastOutput -> lastOutput
                            _ -> -1
                            ) 0 iStates

allPhases :: [[Int]]
allPhases = permutations [0..4]

maxSignal :: [Int] -> Int
maxSignal program =
    foldr (\phases -> \maxOutput -> (runAmps program phases) `max` maxOutput) 0 allPhases

run :: IO ()
run = do
    content <- readFile "src/day7_input.txt"
    let program = str2list $ head (lines content)
    print ("puzzle 1: " ++ show (maxSignal program))
 