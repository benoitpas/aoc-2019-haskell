module Day4
    (
        countDigits, atLeastOnePair, onePair,
        run
    ) where

import Data.Array

inPasswordRange :: [Int] -> Bool
inPasswordRange digits = 
    let p = foldl (\a -> \e -> a * 10 + (e :: Int)) 0 digits
    in 231832 < p && p < 767346

countDigits :: [Int] -> [Int]
countDigits digits = elems $ accumArray (+) 0 (1,9) [(i,1) | i <- digits] 

atLeastOnePair :: [Int] -> Bool
atLeastOnePair digits = (foldr max 0 (countDigits digits)) > 1

onePair :: [Int] -> Bool
onePair digits = length (filter (==2)(countDigits digits)) >= 1

passwords :: [[Int]]
passwords = [[a, b, c, d, e, f] | f<-[1..9], e<-[1..f], d<-[1..e], c<-[1..d], b<-[1..c], a<-[1..b]]

passwordsInRange :: [[Int]]
passwordsInRange = filter inPasswordRange passwords

run :: IO ()
run = 
    do
        print $ ("puzzle 1: " ++ show (length (filter atLeastOnePair passwordsInRange)))
        print $ ("puzzle 2: " ++ show (length (filter onePair passwordsInRange)))
