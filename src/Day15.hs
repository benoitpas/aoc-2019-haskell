module Day15
    (
        run, run2
    ) where

import Day3 (Point)
import Day5 (stateFromProgram, nextStep, State(..))

import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Random as R

import System.Console.ANSI

move :: (Eq a1, Num a1, Num a2, Num a3) => (a3, a2) -> a1 -> (a3, a2)
move (x,y) direction = 
    case direction of
        1 -> (x, y - 1)
        2 -> (x, y + 1)
        3 -> (x + 1, y)
        4 -> (x - 1, y)

nextOutput :: Int -> State -> (State, Integer)
nextOutput nOutput state = until (\(s,c) -> length (output s) == nOutput || c == 99) (\(s, _) -> nextStep s) (state {output = []}, 0)

next :: R.RandomGen g => (State, Point, M.Map Point Char, g, Bool) -> (State, Point, M.Map Point Char, g, Bool)
next (state, droid, area, gen, finished) = 
    let (direction, nGen) = R.randomR (1,4) gen in
    let nDroid = move droid direction in
    let (nState,_) = nextOutput 1 state {input = direction} 
    in case output nState of
            [0] -> (nState, droid, M.insert nDroid '#' area, nGen, finished)
            [1] -> (nState, nDroid, M.insert nDroid '.' area, nGen, finished)
            [2] -> (nState, nDroid, M.insert nDroid 'O' area, nGen, True)

plot :: M.Map Point Char -> IO ()
plot area =
    let lmap = M.toList area in
    let (xlist, ylist) = unzip $ fst $ unzip lmap in
    let (xmin, ymin) = (L.minimum xlist, L.minimum ylist)
        (_   , ymax) = (L.maximum xlist, L.maximum ylist)
    in do
        foldr (\((x,y),c) -> \io -> io >>= (\_ -> do {setCursorPosition (y-ymin) (x-xmin) ; putStr [c]})) clearScreen lmap
        setCursorPosition (ymax - ymin + 1) 0

findPathCount :: State -> Integer -> Point -> (Integer, Point)
findPathCount state prevDirection prevLocation =
    let directions = case prevDirection of
                        1 -> [1,3,4]
                        2 -> [2,3,4]
                        3 -> [1,2,3]
                        4 -> [1,2,4]
                        _ -> [1..4]
    in foldr (\i -> \(a,p) ->   
        if a < 0 then
            let (nState, _) = nextOutput 1 state {input = i} in
            let np = move p i
            in case output nState of
                [0] -> (-1, p)
                [1] -> let (n,p2) = findPathCount nState i np in (if n > 0 then (1 + n,p2) else (-1, np))
                [2] -> (1, np)
        else
                (a, p)) (-1, prevLocation) directions

run2 :: IO ()
run2 = do
    content <- readFile "src/day15_input.txt"
    let iState = stateFromProgram (head (lines content)) 4
    print "The wait can be a couple of minutes"
    let (_, lastLocation, area, _, _) = until (\(_,_,_,_,f)-> f) next (iState, (0,0), M.fromList [((0,0),' ')], R.mkStdGen 31415, False)
    let areaWithDroid = M.insert (0,0) 'D' area
    plot areaWithDroid
    print (show lastLocation)

run :: IO ()
run = do
    content <- readFile "src/day15_input.txt"
    let iState = stateFromProgram (head (lines content)) 4
    let (p1, oxygenLocation) = findPathCount iState 0 (0,0)
    print ("puzzle 1: " ++ show p1)