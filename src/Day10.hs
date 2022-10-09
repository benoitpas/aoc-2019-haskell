module Day10
    (
        Point, toPolar,
        toPoints, aligned, findAligned, findVisibleCount, findMaxVisibleCount,
        anglesAndDistances, findVisibleCount2, listVapAsteroids,
        findPrimes, reduce, anglesAndDistancesInt, listVapAsteroidsInt,
        run
    ) where

import Text.Printf

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

findMaxVisibleCount :: [Point] -> (Int,Point)
findMaxVisibleCount points =
    foldr max (0,(0,0)) (map (\p -> (findVisibleCount2 p points, p)) points)

toPolar :: (Ord b, Floating b) => Point -> (b, b)
toPolar (x,y) =  
    let d = sqrt (fromIntegral(x^2 + y^2)) in
    let (x2,y2) = reduce (x,y) in
    let (x3,y3) = (fromIntegral x2, fromIntegral y2) in
    let d3 = sqrt (x3^2 + y3^2) in
    let a =  if x >= 0 then 
                if y>=0 then asin (y3/d3) else asin (y3/d3)
             else
                if y>=0 then pi - asin (y3/d3) else pi - asin (y3/d3)
    in (a,  d)

intToPolar :: (Floating b, Integral a, Ord b) => (a, a) -> b
intToPolar (x,y) = fst $ toPolar (fromIntegral x, fromIntegral y)

anglesAndDistances :: Point -> [Point] -> [((Double,Double),Point)]
anglesAndDistances (x1,y1) points = 
    L.sort $ L.map (\(x, y) -> 
        let xf = fromIntegral (x - x1)
            yf = fromIntegral (y - y1)
        in (toPolar(xf,yf),(x,y))) (L.delete (x1,y1) points)

findVisibleCount2 :: Point -> [Point] -> Int
findVisibleCount2 p1 points =
    let ad = anglesAndDistances p1 points in
    let adGrouped = L.transpose $ L.groupBy (\((a1,_),_) -> \((a2,_),_) -> a1 == a2) ad
    in length (head adGrouped)

listVapAsteroids :: Point -> [Point] -> [Point]
listVapAsteroids p1 points =
    let ad = anglesAndDistances p1 points in
    let adGrouped = L.transpose $ L.groupBy (\((a1,_),_) -> \((a2,_),_) -> a1 == a2) ad
    in adGrouped >>= map (\(_,p) -> p)


findPrimes :: Integral a => a -> [a]
findPrimes m = 
    let next (primes,numbers) = case numbers of
                                    n:remain -> next (primes ++ [n], remain L.\\ (map (*n) [1..m `div` n]))
                                    _ -> (primes,numbers)
    in fst $ next ([], [2 .. m])

primes = findPrimes 100
reduce :: (Int,Int) -> (Int,Int)
reduce (a,b) = 
    let (a2,b2) = case (a == 0, b == 0) of
                    (True, False) -> (a, b `div` abs b)
                    (False, True) -> (a `div` abs a, b)
                    _ -> (a,b)
        p = takeWhile (<= min (abs a2) (abs b2)) primes
        usePrime p (a,b) = if a `mod` p == 0 && b `mod` p == 0 then usePrime p (a `div` p, b `div` p) else (a,b)
    in L.foldl (\(a,b) -> \n -> usePrime n (a,b)) (a2,b2) p

anglesAndDistancesInt :: Point -> [Point] -> [((Int,Int),Int,Point)]
anglesAndDistancesInt (x1,y1) points = 
    let sortedPoints = L.sortBy (\(x2,y2) -> \(x3,y3) -> compare (toPolar(fromIntegral (x2-x1), fromIntegral (y2-y1))) (toPolar(fromIntegral (x3-x1), fromIntegral (y3-y1)))) (L.delete (x1,y1) points) in
    let r = L.map (\(x, y) -> let xa = x - x1
                                  ya = y - y1
                              in (reduce(xa,ya), xa^2 + ya^2, (x,y))) sortedPoints
    in r

listVapAsteroidsInt p1 points =
    let ad = anglesAndDistancesInt p1 points in
    let adGrouped = L.groupBy (\((a1,b1),_,_) -> \((a2,b2),_,_) -> a1 == a2 && b1 == b2) ad
        -- sort the groups
    --let adGroupedSorted1 = L.sortBy (\((_,d1,_):_) -> \((_,d2,_):_) -> compare d1 d2) adGrouped in adGroupedSorted1
        -- sort inside the groups
    --let adGroupedSorted2 = L.map (L.sortBy (\((a1,b1),d1,_) -> \((a2,b2),d2,_) -> compare (intToPolar(a1,b1),d1) (intToPolar(a2,b2),d2))) adGroupedSorted1
    in L.transpose adGrouped >>= map (\(_,_,p) -> p)

run :: IO ()
run = do
    content <- readFile "src/day10_input.txt"
    let points = toPoints (lines content)
    let mvc = findMaxVisibleCount points
    print ("puzzle 1: " ++ show (fst mvc))
    let p2 = (listVapAsteroids (snd mvc) points) !! 199
    print ("puzzle 2: " ++ show (fst p2 * 100 + snd p2))
