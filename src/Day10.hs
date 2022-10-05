module Day10
    (
        Point,
        toPoints, aligned, findAligned, findVisibleCount, findMaxVisibleCount,
        run
    ) where

import qualified Data.Set as S
import qualified Data.List as L
type Point = (Int,Int)

toPoints :: [String] -> [Point]
toPoints rows = 
    let addIndex l = [0..(length l - 1)] `zip` l in
    let wIndex = addIndex (map addIndex rows)
    in wIndex >>= \(y,row) -> row >>= (\(x,c) -> if c=='#' then [(x,y)] else [])

aligned :: Point -> Point -> Point -> Bool
aligned (x1,y1) (x2,y2) (x3,y3) = (x1-x2)*(y2-y3) - (x2-x3) * (y1-y2) == 0 && (x3,y3) /= (x1,y1) && (x3,y3) /= (x2,y2)

findAligned :: Point -> ([Point], S.Set Point, S.Set Point) -> (([Point], S.Set Point, S.Set Point))
findAligned p1 (todo, visible, hidden) = 
    case todo of
        p2:remain -> let alignedPoints = foldl  (\a -> \p3 -> (if aligned p1 p2 p3 then p3:a else a)) [p2] remain in 
                let alignedPointsWithDistance = L.sortOn (\(x,y) -> (x - fst p1)^2 + (y - snd p1)^2) alignedPoints in
                let (cx,cy) = head alignedPointsWithDistance in
                let (part1,part2) = L.partition (\(x,y) -> (cx - fst p1)*(x-cx) >=0 && (cy -snd p1) * (y-cy) >=0) alignedPointsWithDistance in
                let headList l = if length l > 0 then  [head l] else [] in
                let tailList l = if length l > 0 then  tail l else []
                in (L.foldr L.delete todo alignedPointsWithDistance,
                    S.fromList (headList part1 ++ headList part2) `S.union` visible, 
                    hidden `S.union` S.fromList (tailList part1 ++ tailList part2))
        _ -> (todo, visible, hidden)

findVisibleCount :: Point -> [Point] -> Int
findVisibleCount p1 points = 
    let todo = L.delete p1 points in
    let (_,visible,_) = head (filter (\(t,_,_) -> length t == 0) (iterate (findAligned p1) (todo, S.empty, S.empty))) in
        length visible

findMaxVisibleCount :: [Point] -> Int
findMaxVisibleCount points =
    foldr max 0 (map (\p -> findVisibleCount p points) points)

run :: IO ()
run = do
    content <- readFile "src/day10_input.txt"
    let m = (lines content)
    print ("puzzle 1: " ++ show (findMaxVisibleCount (toPoints m)))
