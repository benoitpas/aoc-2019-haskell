module Day13
    (
        toTiles,
        run
    ) where

import qualified Data.Set as S
import qualified Data.List as L

import Day3 (Point)
import Day5 (runProgram, State(..))
import Day7 (nextSteps)

toTiles [] = []
toTiles codes = ((x,y), tile):(toTiles remain)
                    where (x:y:tile:_,remain) = L.splitAt 3 codes

run :: IO ()
run = do
    content <- readFile "src/day13_input.txt"
    let o = runProgram (head (lines content)) 0
    let tiles = toTiles o
    let blocks = filter (\(_,t) -> t == 2) tiles
    print ("puzzle 1: " ++ show (length blocks))