module Day2
    ( run, wordsWhen, str2array
    ) where

import Data.Array

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

str2array:: String -> Array Int Int
str2array s = 
    let strings = map read (wordsWhen (==',') s) in
    listArray (0,(length strings) - 1) strings

nextStep :: ((Array Int Int),Int) -> ((Array Int Int),Int)
nextStep (program,index) = (program ,index)

run :: IO ()
run = do
    content <- readFile "src/day2_input.txt"
    let program = str2array (head (lines content))
    print program
