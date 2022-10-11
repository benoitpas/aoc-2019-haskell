module Day11
    (
        nextDirection,
        run
    ) where

import qualified Data.Map as M
import Day3 (Point)
import Day5 (stateFromProgram, State(..))
import Day7 (nextSteps)

data RobotState = RobotState {
    state :: State,
    location :: Point,
    direction :: Point
}

type Hull = M.Map Point Int

hullColor :: Point -> Hull -> Int
hullColor p hull = M.findWithDefault 0 p hull

nextDirection :: Point -> Integer -> Point
nextDirection (dx,dy) turn = 
    let left = 0
        right = 1 in
    let m = if (turn == left && dx /= 0) || (turn == right && dy /= 0) then -1 else 1
    in (m * dy, m * dx)

nextLocation :: (RobotState, Hull, Bool) -> (RobotState, Hull, Bool)
nextLocation (rs,hull,_) =
    let hc = hullColor (location rs) hull in
    let (nState1,_) = nextSteps (state rs) { input = toInteger hc} in
    let color = fromIntegral (last (output nState1)) in
    let (nState2, isFinished) = nextSteps nState1 in
    let turn = last (output nState2) in
    let (nDirectionX, nDirectionY) = nextDirection (direction rs) turn in
    let nLocation = (fst (location rs) + nDirectionX, snd (location rs) + nDirectionY)
    in (RobotState nState2{output = []} nLocation (nDirectionX,nDirectionY), M.insert (location rs) color hull, isFinished)

run :: IO ()
run = do
    content <- readFile "src/day11_input.txt"
    let iState0 = stateFromProgram (head (lines content)) 0
    let iHull = M.empty
    let rs0 = RobotState iState0 (0,0) (0,-1)

    let s = iterate nextLocation (rs0, iHull, False)
    let s2 = take 10500 s
    let (_,h,_) = last s2
    print ("puzzle 1: " ++ show (length h))