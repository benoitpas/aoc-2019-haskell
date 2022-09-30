module Day2
    (
        run, Memory,
        wordsWhen, str2list, list2array, str2array,
        allSteps, nextStep
    ) where

import Data.Array

type Memory =  Array Integer Integer

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

str2list :: String -> [Integer]
str2list s = map read (wordsWhen (==',') s)

list2array :: [Integer] -> Array Integer Integer
list2array lst = listArray (0,  toInteger (length lst)- 1) lst

str2array :: String -> Memory
str2array = list2array . str2list

nextStep :: (Memory,Integer) -> (Memory,Integer)
nextStep (program, idx) = 
    case (program ! idx) of
        1  -> process (+)
        2  -> process (*)
        _ -> (program, -1 - idx)
    where
        process op =
            let a = (program ! (program ! (idx + 1))) in
            let b = (program ! (program ! (idx + 2))) in
                (program // [(program ! (idx + 3), a `op` b)], idx + 4)

allSteps :: (Memory,Integer) -> (Memory,Integer)
allSteps (program, idx) = last ((takeWhile (\(_,i) -> i >= 0) (iterate nextStep (program, idx))))

runProgram :: Memory -> Integer -> Integer -> Integer
runProgram memory noun verb = 
    let memory2 = memory // [(1, noun), (2, verb)] in
    fst (allSteps (memory2,0)) ! 0

findOutput :: Memory -> Integer -> Integer 
findOutput memory output =
    let solutions = [(noun,verb) | noun <- [0..99], verb <- [0..99], runProgram memory noun verb == output] in
    case solutions of
        [(noun,verb)] -> 100 * noun + verb
        _ -> 0


run :: IO ()
run = do
    content <- readFile "src/day2_input.txt"
    let memory = str2array (head (lines content))
    let p1 = runProgram memory 12 2
    print ("puzzle 1: " ++ show p1)
    let p2 = findOutput memory 19690720
    print ("puzzle 2: " ++ show p2)
