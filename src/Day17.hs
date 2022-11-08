module Day17
    (
        sumAligments, toMap,
        run
    ) where

import Day3 (Point)
import Day5 (stateFromProgram, nextStep, State(..))

import Data.Char
import qualified Data.Map as M

sumAligments :: M.Map Point Char -> Int
sumAligments m = M.foldrWithKey (\(x,y) _  acc -> 
    let isScaffolding p = (((M.findWithDefault ' ' p m)) == '#') in
    let isIntersection (x,y) = isScaffolding (x,y) && isScaffolding (x-1,y) && isScaffolding (x+1,y) && isScaffolding (x,y-1) && isScaffolding (x,y+1)
    in acc + if isIntersection (x,y) then x * y else 0) 0 m 

toMap :: [Integer] -> M.Map Point Char
toMap m = fst $ foldl (\(m,(x,y)) c -> case c of
                                        10 -> (m, (0,y+1))
                                        _ -> (M.insert (x,y) (chr (fromIntegral c)) m, (x+1,y))) 
                      (M.empty, (0,0)) m

run :: IO ()
run = do
    content <- readFile "src/day17_input.txt"
    let program = head $ lines content

    let iState = stateFromProgram program 0
    let (state, _) = until (\(_,c) -> c == 99) (\(s,_) -> nextStep s) (iState, 0)
    let s = map (chr . fromIntegral) (output state)
    putStrLn s
    let m = toMap (output state)
    print ("puzzle 1 : " ++ show (sumAligments m))