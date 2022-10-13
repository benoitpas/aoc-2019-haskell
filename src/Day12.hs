module Day12
    (
        updateVelocity1dim, updateVelocity, nextStep, computeEnergy,
        run
    ) where

import qualified Data.List as L

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

computeEnergy :: [[Int]] -> Int -> Int
computeEnergy locations nbSteps =
    let velocity = map (map (\_ -> 0)) locations in
    let lv = iterate nextStep (locations, velocity) in 
    let (nLocations, nVelocity) = lv !! nbSteps in
    let sumRows rows = map (foldr (\v -> \a -> abs v + a) 0) rows
    in foldl (\a -> \(x,y) -> a + x * y) 0 (zip (sumRows nLocations) (sumRows nVelocity))
    
run :: IO ()
run = do
    let e = computeEnergy iLocations 1000
    print ("puzzle 1: " ++ show e)
