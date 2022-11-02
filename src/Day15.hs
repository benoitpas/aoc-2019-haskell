module Day15
    (
        run
    ) where

import Day3 (Point)
import Day5 (stateFromProgram, allSteps, nextStep, State(..))
import Day13 (nextOutput)

import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Random as R

move (x,y) direction = 
    case direction of
        1 -> (x, y - 1)
        2 -> (x, y + 1)
        3 -> (x + 1, y)
        4 -> (x - 1, y)

next :: R.RandomGen g => (State, Point, M.Map Point Char, g, Bool) -> (State, Point, M.Map Point Char, g, Bool)
next (state, droid, area, gen, finished) = 
    let (direction, nGen) = R.randomR (1,4) gen in
    let nDroid = move droid direction in
    let (nState1,_) = nextOutput 0 state {output = [], input = direction} in
    let (nState2, _) = nextStep nState1
    in case output nState2 of
            [0] -> (nState2, droid, M.insert nDroid '#' area, nGen, finished)
            [1] -> (nState2, nDroid, M.insert nDroid '.' area, nGen, finished)
            [2] -> (nState2, nDroid, M.insert nDroid 'O' area, nGen, True)

plot :: M.Map Point Char -> IO ()
plot area = putStr ""

run :: IO ()
run = do
    content <- readFile "src/day15_input.txt"
    let iState = stateFromProgram (head (lines content)) 4
    let (_, lastLocation, area, _, _) = until (\(_,_,_,_,f)-> f) next (iState, (0,0), M.fromList [((0,0),' ')], R.mkStdGen 31415, False)
    print (show area)
    print (show lastLocation)

