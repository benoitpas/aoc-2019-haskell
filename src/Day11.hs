module Day11
    (
        nextDirection, mapToGrid,
        run
    ) where

import qualified Data.Map as M
import qualified Data.List as L

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

mapToGrid :: Hull -> [String]
mapToGrid h = 
    let hullValue x y = case h M.!? (x,y) of
                            Just 1 -> '#'
                            _ -> ' ' in
    let e = M.keys h in
    let (xlist,ylist) = (map fst e,  map snd e) in
    let (xmin, xmax) = (L.minimum xlist, L.maximum xlist)
        (ymin, ymax) = (L.minimum ylist, L.maximum ylist)
    in map (\y -> (map (\x -> hullValue x y) [xmin..xmax])) [ymin..ymax]

hullPoints :: State -> Hull -> Int -> Hull
hullPoints iState iHull nbIteration =
    let rs = RobotState iState (0,0) (0,-1) in
    let s = iterate nextLocation (rs, iHull, False) in
    let s2 = take nbIteration s in
    let (_,h,_) = last s2 
    in h

run :: IO ()
run = do
    content <- readFile "src/day11_input.txt"
    let iState0 = stateFromProgram (head (lines content)) 0
    let iHull = M.empty
    let h = hullPoints iState0 iHull 10500
    print ("puzzle 1: " ++ show (length h))

    let hull = M.fromList [((0,0),1)]
    let h2 = hullPoints iState0 hull 250
    let g2 = mapToGrid h2
    print ("puzzle 2:")
    putStrLn (L.unlines g2)