module Day10
    (
        Point, toPolar,
        toPoints, findVisibleCount, findMaxVisibleCount,
        anglesAndDistances, listVapAsteroids,
        findPrimes, reduce,
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

findMaxVisibleCount :: [Point] -> (Int,Point)
findMaxVisibleCount points =
    foldr max (0,(0,0)) (map (\p -> (findVisibleCount p points, p)) points)

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

anglesAndDistances :: Point -> [Point] -> [((Double,Double),Point)]
anglesAndDistances (x1,y1) points = 
    L.sort $ L.map (\(x, y) -> 
        let xf = fromIntegral (x - x1)
            yf = fromIntegral (y - y1)
        in (toPolar(xf,yf),(x,y))) (L.delete (x1,y1) points)

findVisibleCount :: Point -> [Point] -> Int
findVisibleCount p1 points =
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

initialPrimes :: [Int]
initialPrimes = findPrimes 100

reduce :: (Int,Int) -> (Int,Int)
reduce (a,b) = 
    let (a2,b2) = case (a == 0, b == 0) of
                    (True, False) -> (a, b `div` abs b)
                    (False, True) -> (a `div` abs a, b)
                    _ -> (a,b)
        p = takeWhile (<= min (abs a2) (abs b2)) initialPrimes
        usePrime p (a,b) = if a `mod` p == 0 && b `mod` p == 0 then usePrime p (a `div` p, b `div` p) else (a,b)
    in L.foldl (\(a,b) -> \n -> usePrime n (a,b)) (a2,b2) p

run :: IO ()
run = do
    content <- readFile "src/day10_input.txt"
    let points = toPoints (lines content)
    let mvc = findMaxVisibleCount points
    print ("puzzle 1: " ++ show (fst mvc))
    let p2 = (listVapAsteroids (snd mvc) points) !! 199
    print ("puzzle 2: " ++ show (fst p2 * 100 + snd p2))
