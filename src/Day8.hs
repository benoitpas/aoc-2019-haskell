module Day8
    (
        split, toImages, countDigits, toImage,
        run
    ) where

import Data.Char(digitToInt)
import Data.List(intercalate, sort, transpose)

split :: Int -> [a] -> [[a]]
split n str = 
    if length str <= n then 
        [str] 
    else
        let pair = splitAt n str in (fst pair):split n (snd pair)

toImages :: Int -> Int -> String -> [[Int]]
toImages x y imageData = split (x * y) (map digitToInt imageData)

countDigits :: Int -> [Int] -> Int
countDigits d image = length (filter (==d) image)

findColor :: [Int] -> Int
findColor colors = foldl (\a -> \e -> if a == 2 then e else a) 2 colors

toAsc :: (Eq a, Num a) => a -> Char
toAsc 1 = '*'
toAsc _ = ' '

toImage :: [[Int]] ->[Int]
toImage images = map findColor (transpose images)

run :: IO ()
run = do
    content <- readFile "src/day8_input.txt"
    let imageData = head (lines content)
    let images = (toImages 25 6 imageData)
    let i0 = snd . head . sort $ map (\i -> (countDigits 0 i, i)) images
    print $ "puzzle 1: " ++ show (countDigits 1 i0 * countDigits 2 i0)
    let image = split 25 (map toAsc (toImage images))
    putStrLn $ "puzzle 2: " 
    putStrLn $  intercalate  "\n" image