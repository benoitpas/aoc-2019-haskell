module Day18
    (
        possibleDirections,
        findKeys,
        shortestPath,
        run
    ) where

import Day17(toMap, toInts)

import Data.Char
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

checkLocation :: (Int, Int) -> M.Map (Int, Int) Char -> Char
checkLocation = M.findWithDefault '#'

isFree area bag (x,y) = 
    let c = checkLocation (x,y) area 
    in c == '.' || c == '@' || isLower c || (toLower c) `S.member` bag

possibleDirections area (sx,sy) bag (dx,dy) = 
    let directions = L.delete (-dx,-dy) [(0,1), (1,0), (0,-1),(-1,0)]  in
    directions  >>= (\(pdx, pdy) ->  if isFree area bag (sx+pdx,sy+pdy) then [(pdx,pdy)] else [])

findKeys area (sx,sy) bag (dx,dy) iDistance =
    let pd = possibleDirections area (sx,sy) bag (dx,dy) in
    let nDistance = iDistance + 1
    in pd >>= (\(pdx, pdy) -> 
        let (nsx,nsy) = (sx+pdx, sy+pdy) in
        let c = checkLocation (nsx,nsy) area
        in case (isLower c, c `S.member` bag) of
                    (True, False) -> [(c,nDistance)]
                    _ -> findKeys area (nsx,nsy) bag (pdx,pdy) nDistance)

shortestPath l = 
    let m = toMap (toInts l) in
    let start = fst $ head $ M.toList (M.filterWithKey (\_ c -> c == '@') m)
    in findKeys m start S.empty (0,0) 0

run :: IO ()
run = do
    content <- readFile "src/day18_input.txt"
    putStrLn content
    print (show (shortestPath (lines (content))))