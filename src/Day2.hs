module Day2
    (
        run,
        wordsWhen, list2array, str2array, allSteps,
        nextStep
    ) where

import Data.Array

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

list2array :: Ix i => [i] -> Array Int i
list2array lst = listArray (0,(length lst) - 1) lst

str2array :: String -> Array Int Int
str2array s = 
    let ints = map read (wordsWhen (==',') s) in
    list2array ints

nextStep :: ((Array Int Int),Int) -> ((Array Int Int),Int)
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

allSteps :: ((Array Int Int),Int) -> ((Array Int Int),Int)
allSteps (program, idx) = last ((takeWhile (\(_,i) -> i >= 0) (iterate nextStep (program, idx))))

run :: IO ()
run = do
    content <- readFile "src/day2_input.txt"
    let program = str2array (head (lines content))
    let fixedProgram = program // [(1,12),(2,2)]
    let p1 = fst (allSteps (fixedProgram,0)) ! 0
    print ("puzzle 1: " ++ show p1)
