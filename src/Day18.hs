module Day18
    (
        possibleDirections,
        findKeys,
        shortestPath,
        run
    ) where

import Day3 (Point)
import Day17(toMap, toInts)

import Data.Char
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

type Area = M.Map Point Char

checkLocation :: Point -> Area -> Char
checkLocation = M.findWithDefault '#'

isFree :: Foldable t => Area -> t Char -> Point-> Bool
isFree area bag (x,y) = 
    let c = checkLocation (x,y) area 
    in c == '.' || c == '@' || isLower c || (toLower c) `L.elem` bag

possibleDirections :: Foldable t => Area -> Point -> t Char -> S.Set Point -> [Point]
possibleDirections area (sx,sy) bag prevLocations =
    let directions = [(0,1), (1,0), (0,-1),(-1,0)] in
    directions  >>= (\(pdx, pdy) -> let (nx,ny)= (sx+pdx,sy+pdy)
                                    in if isFree area bag (nx,ny) && S.notMember (nx,ny) prevLocations
                                       then [(pdx,pdy)] else [])

findKeys :: Area -> Point -> S.Set Char -> S.Set Point -> Int -> [(Char, Int, Point)]
findKeys area (sx,sy) bag prevLocations iDistance =
    let pd = possibleDirections area (sx,sy) bag prevLocations in
    let nDistance = iDistance + 1
    in pd >>= (\(pdx, pdy) -> 
        let nsp = (sx+pdx, sy+pdy) in
        let c = checkLocation nsp area
        in case (isLower c, c `L.elem` bag) of
                    (True, False) -> [(c, nDistance, nsp)]
                    _ -> findKeys area nsp bag (S.insert nsp prevLocations) nDistance)

type Todo = M.Map (Char, S.Set Char) (Int, (Int, Int))
findKeys2 :: Area -> (Int, Int) -> S.Set Char -> Int -> Todo
findKeys2 area (sx,sy) bag iDistance =
    let r = findKeys area (sx,sy) bag (S.fromList[(sx,sy)]) iDistance
    in foldr (\(c,d,p) a -> let nd = case M.lookup (c, bag) a of
                                                    Just (ad,_) -> min d ad
                                                    _ -> d in M.insert (c,bag) (nd,p) a) M.empty r

findNextNode :: Todo -> ((Char, S.Set Char),Int, (Int,Int))
findNextNode todo =
    let l = L.sortOn (\(_,(d,_)) -> d) (M.toList todo) in
    let (k,(d,p)) = head l
    in (k,d,p)

type Done = M.Map (Char, S.Set Char) Int
nextNode :: Area -> (Todo, Done) -> (Todo, Done)
nextNode area (todo,done) =
    let (nkey,nd,np) = findNextNode todo in
    let keys = findKeys2 area np (S.insert (fst nkey) (snd nkey)) nd in
    let ndone = M.insert nkey nd done in
    let ntodo = M.delete nkey todo
    in  M.foldrWithKey (\k (d,p) (td,dn) ->
        case (M.member k done, M.lookup k todo) of
            (False, Nothing) -> (M.insert k (d,p) td,dn)
            (False, Just (tdd, _)) -> (if d<tdd then M.insert k (d,p) td else td,dn)
            (_, _) -> (td,dn)) (ntodo, ndone) keys
    
shortestPath :: [String] -> Int
shortestPath l =
    let m = toMap (toInts l) in
    let start = fst $ head $ M.toList (M.filterWithKey (\_ c -> c == '@') m) in
    let todo = findKeys2 m start S.empty 0 in
    let (_,done) = until (\(td,_) ->(length (M.elems td) == 0)) (nextNode m) (todo, M.fromList []) in
    let nbKeys = length $ filter isLower (M.elems m) in
    let (_,r) = head $ L.sortOn  (\(_,d) -> d) (filter (\((_,bag),_) -> length bag == (nbKeys - 1)) (M.toList done))
    in r

run :: IO ()
run = do
    content <- readFile "src/day18_input.txt"
    putStrLn content
    print (show (shortestPath (lines (content))))