module Day1
    ( run,
    fuel1, fuel2
    ) where

fuel1 :: Int -> Int
fuel1 m = m `div` 3 - 2

fuel2 :: Int -> Int
fuel2 m = sum(drop 1 (takeWhile ((<) 0) (iterate fuel1 m)))

run :: IO ()
run = do
    content <- readFile "src/day1_input.txt"
    let numbers = (map read (lines content))  :: [Int]
    let s1 =  sum (map fuel1 numbers)
    putStrLn (show s1)
