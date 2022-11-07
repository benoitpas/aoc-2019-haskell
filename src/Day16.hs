module Day16
    (
        pattern, onePhase, output,
        onePhase2, output2,
        run
    ) where

import qualified Data.List as L

pattern :: Num a => Int -> [a]
pattern index = 
    let p = [0, 1, 0, -1] >>= replicate index
    in tail (cycle p)


onePhase :: [Int] -> [Int]
onePhase digits = 
    map (\i -> abs(L.foldl' (\acc (a,b) -> acc + a * b ) 0 (digits `zip` pattern i)) `mod` 10 ) [1..length digits]

readInteger:: Char -> Int
readInteger c = read [c]

output:: String -> Int -> String
output input nbPhases =
    let digits = map readInteger input in
    let r = last $ take (nbPhases + 1) (iterate onePhase digits)
    in take 8 $ map (head . show) r

onePhase2 :: [Int] -> [Int]
onePhase2 digits =  init $ foldr (\d a -> ((d + head a) `mod` 10):a) [0] digits

output2:: String -> Int -> String
output2 input nbPhases =
    let input2 = take (10000 * length input) $ (concat . repeat) input in
    let index = read $ take 7 input2 :: Int in
    let digits = map readInteger input2 in
    let r = last $ take (nbPhases + 1) (iterate onePhase2 (drop index digits))
    in take 8 $ map (head . show) r

run :: IO ()
run = do
    content <- readFile "src/day16_input.txt"
    let input = head (lines content)
    let p1 = output input 100
    print ("puzzle 1=" ++ p1)
    let p2 = output2 input 100
    print ("puzzle 2=" ++ p2)
