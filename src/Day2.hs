module Day2
    (
        run, Memory,
        wordsWhen, str2list, list2memory, str2memory,
        allSteps, nextStep
    ) where

import Data.Map

type Memory =  Map Integer Integer

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

str2list :: String -> [Integer]
str2list s = Prelude.map read (wordsWhen (==',') s)

list2memory :: [Integer] -> Memory
list2memory lst =  let n = toInteger (length lst) in
    fromList ([0..(n-1)] `zip` lst)

str2memory :: String -> Memory
str2memory = list2memory . str2list

nextStep :: (Memory, Integer) -> (Memory, Integer)
nextStep (program, idx) = 
    case (program ! idx) of
        1  -> process (+)
        2  -> process (*)
        _ -> (program, -1 - idx)
    where
        process op =
            let a = (program ! (program ! (idx + 1))) in
            let b = (program ! (program ! (idx + 2))) in
                (insert (program ! (idx + 3))  (a `op` b) program, idx + 4)

allSteps :: (Memory,Integer) -> (Memory,Integer)
allSteps (program, idx) = last ((takeWhile (\(_,i) -> i >= 0) (iterate nextStep (program, idx))))

runProgram :: Memory -> Integer -> Integer -> Integer
runProgram memory noun verb = 
    let memory2 = memory `union` fromList [(1, noun), (2, verb)] in
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
    let memory = str2memory (head (lines content))
    let p1 = runProgram memory 12 2
    print ("puzzle 1: " ++ show p1)
    let p2 = findOutput memory 19690720
    print ("puzzle 2: " ++ show p2)
