module Day17
    (
        sumAligments, toMap, toInts,
        run
    ) where

import Day3 (Point)
import Day5 (stateFromProgram, nextStep, State(..))

import Data.Char
import qualified Data.Map as M

sumAligments :: M.Map Point Char -> Int
sumAligments m = M.foldrWithKey (\(x,y) _  acc -> 
    let isScaffolding p = (((M.findWithDefault ' ' p m)) == '#') in
    let isIntersection (ix,iy) = isScaffolding (ix, iy) && isScaffolding (ix-1, iy) && isScaffolding (ix+1, iy)
                                 && isScaffolding (ix, iy-1) && isScaffolding (ix, iy+1)
    in acc + if isIntersection (x,y) then x * y else 0) 0 m 

toMap :: [Integer] -> M.Map Point Char
toMap l = fst $ foldl (\(m,(x,y)) c -> case c of
                                        10 -> (m, (0,y+1))
                                        _ -> (M.insert (x,y) (chr (fromIntegral c)) m, (x+1,y))) 
                      (M.empty, (0,0)) l

toInts :: [String] -> [Integer]
toInts ints = ints >>= (\l -> map (toInteger . ord) l ++ [10])

program2 :: [Integer]
program2 = toInts [
    "A,B,A,A,B,C,B,C,C,B",
    "L,12,R,8,L,6,R,8,L,6",
    "R,8,L,12,L,12,R,8",
    "L,6,R,6,L,12",
    "n"]

runUntilCmd :: Integer -> State -> (State, Integer)
runUntilCmd cmd state = until (\(_,c) -> c == cmd) (\(s,_) -> nextStep s) (state, 0)

processInput :: [Integer] -> State -> State
processInput inputs iState =
    foldl (\state i -> fst $ runUntilCmd 3 state {input = i}) iState inputs

run :: IO ()
run = do
    content <- readFile "src/day17_input.txt"
    let program = head $ lines content

    let iState = stateFromProgram program 0
    let (state, _) = runUntilCmd 99 iState
    let m = toMap (output state)
    print ("puzzle 1 : " ++ show (sumAligments m))

    let iState2 = iState {memory = M.insert 0 2 (memory iState)}
    let afterInputState = processInput program2 iState2
    let s1 = map (chr . fromIntegral) (output afterInputState)
    putStrLn s1
    let fState = fst $ runUntilCmd 99 afterInputState
    print ("puzzle 2 : " ++ show (last (output fState)))

