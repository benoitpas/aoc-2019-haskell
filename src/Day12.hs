module Day12
    (
        updateVelocity1dim, updateVelocity, nextStep,
        computeEnergy, findRepeat, findRepeatCol, reduce,
        run
    ) where

import qualified Data.List as L
import Day10 (findPrimes)
-- <x=9, y=13, z=-8>
-- <x=-3, y=16, z=-17>
-- <x=-4, y=11, z=-10>
-- <x=0, y=-2, z=-2>

iLocations :: [[Int]]
iLocations = [
    [ 9, 13, -8],
    [-3, 16, -17],
    [-4, 11, -10],
    [ 0, -2, -2]]


updateVelocity1dim :: ([Int],Int,Int) -> Int 
updateVelocity1dim (locations,location,velocity) = 
    foldr (\cLocation -> \uVelocity -> 
        if cLocation < location then uVelocity - 1
        else if location < cLocation then uVelocity + 1
        else uVelocity) velocity locations

updateVelocity :: [[Int]] -> ([Int],[Int]) -> [Int]
updateVelocity locations (location,velocity) = 
    let i = zip3 (L.transpose locations) location velocity
    in map updateVelocity1dim i

nextStep :: ([[Int]],[[Int]]) -> ([[Int]],[[Int]])
nextStep (locations,velocity) =
    let nVelocity = map (updateVelocity locations) (zip locations velocity) in
    let nLocations = map (\(l1,l2) -> map (\(a,b) -> a + b) (zip l1 l2)) (zip locations nVelocity)
    in (nLocations, nVelocity)

locationsVelocity :: [[Int]] -> [([[Int]],[[Int]])]
locationsVelocity locations =
    let velocity = map (map (\_ -> 0)) locations
    in iterate nextStep (locations, velocity)

computeEnergy :: [[Int]] -> Int -> Int
computeEnergy locations nbSteps =
    let (nLocations, nVelocity) = (locationsVelocity locations) !! nbSteps in
    let sumRows rows = map (foldr (\v -> \a -> abs v + a) 0) rows
    in foldl (\a -> \(x,y) -> a + x * y) 0 (zip (sumRows nLocations) (sumRows nVelocity))

findRepeat :: [[Int]] -> Int
findRepeat locations =
    let lv = locationsVelocity locations in
    let velocity = snd (lv !! 0)
    in 1 + length (takeWhile (/=(locations,velocity)) (tail lv))

findRepeatCol :: [[Int]] -> Int -> Int
findRepeatCol locations col =
    let colLocations = map (\row -> [row !! col]) locations
    in findRepeat colLocations

findRepeat2 :: [[Int]] -> [Int]
findRepeat2 locations = map (findRepeatCol locations) [0..(length (head locations) - 1)]

reduce :: [Int] -> [Int]
reduce ints =
    --let m = foldl min (head l) l in
    let primes = findPrimes 10
        usePrime p l = if (foldr  (\n -> \a -> a && (n `mod` p == 0)) True l) then usePrime p (map (`div` p) l) else l
    in L.foldl (\l -> \p -> usePrime p l) ints primes

run :: IO ()
run = do
    let e = computeEnergy iLocations 1000
    print ("puzzle 1: " ++ show e)
    let l2 = reduce (findRepeat2 iLocations)
    let r2 = foldl (\a -> \b -> toInteger a * toInteger b) 1 l2
    print ("puzzle 2: " ++ show r2)
