module Day10
    (
        toPoints, aligned, findAligned,
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
                let alignedPointsWithDistance = L.sortOn (\(x,y) -> (x - fst p1)^2 + (y - snd p1)^2) alignedPoints
                in (L.foldr L.delete todo alignedPointsWithDistance,
                    S.insert (head alignedPointsWithDistance) visible, 
                    hidden `S.union` S.fromList (tail alignedPointsWithDistance))
        _ -> (todo, visible, hidden)
   

run :: IO ()
run = do
    content <- readFile "src/day10_input.txt"
    let m = (lines content)
    print ("puzzle 1: " ++ show (toPoints m))
