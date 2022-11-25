module Day18
    (
        possibleDirections,
        findKeysRec,
        shortestPath,
        shortestPath2,
        run
    ) where

import Day3 (Point)
import Day17(toMap, toInts)

import Data.Char
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock

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

findKeysRec :: Area -> Point -> S.Set Char -> S.Set Point -> Int -> [(Char, Int, Point)]
findKeysRec area (sx,sy) bag prevLocations iDistance =
    let pd = possibleDirections area (sx,sy) bag prevLocations in
    let nDistance = iDistance + 1
    in pd >>= (\(pdx, pdy) -> 
        let nsp = (sx+pdx, sy+pdy) in
        let c = checkLocation nsp area
        in case (isLower c, c `L.elem` bag) of
                    (True, False) -> [(c, nDistance, nsp)]
                    _ -> findKeysRec area nsp bag (S.insert nsp prevLocations) nDistance)


--type Todo = M.Map (Point, S.Set Char) Int
--findKeys :: Area -> Point -> S.Set Char -> Int -> Todo
findKeys area p bag iDistance =
    let keys = findKeysRec area p bag (S.fromList [p]) iDistance
    in foldr (\(c,d,p) a -> let nd = case M.lookup (p, bag) a of
                                                    Just ad -> min d ad
                                                    _ -> d in M.insert (p,bag) nd a) M.empty keys

--findNextNode :: Todo -> ((Point, S.Set Char), Int)
findNextNode todo =
    let l = L.sortOn (\(_,d) -> d) (M.toList todo) in -- could be replaced by snd ?
    let (k,d) = head l
    in (k,d)

--type Done = M.Map (Point, S.Set Char) Int
--nextNode :: Area -> (Todo, Done) -> (Todo, Done)
nextNode area (todo,done) =
    let (nkey,nd) = findNextNode todo in
    let (np, bag) = nkey in
    let nbag = S.insert (checkLocation np area) bag in
    let keys = findKeys area np nbag nd in
    let ndone = M.insert nkey nd done in
    let ntodo = M.delete nkey todo
    in  M.foldrWithKey (\k d (td,dn) ->
        case (M.member k done, M.lookup k todo) of
            (False, Nothing) -> (M.insert k d td,dn)
            (False, Just tdd) -> (if d<tdd then M.insert k d td else td,dn)
            (_, _) -> (td,dn)) (ntodo, ndone) keys
    
--shortestPath :: [String] -> Int
shortestPath l =
    let m = toMap (toInts l) in
    let start = fst $ head $ M.toList (M.filterWithKey (\_ c -> c == '@') m) in
    let todo = findKeys m start S.empty 0 in
    let (_,done) = until (\(td,_) ->(length (M.elems td) == 0)) (nextNode m) (todo, M.fromList []) in
    let nbKeys = length $ filter isLower (M.elems m) in
    let (_,r) = head $ L.sortOn  (\(_,d) -> d) (filter (\((_,bag),_) -> length bag == (nbKeys - 1)) (M.toList done))
    in r

type Todo = M.Map (S.Set Point, S.Set Char) Int
findKeys2 :: Area -> S.Set Point -> S.Set Char -> Int -> Todo
findKeys2 area points bag iDistance =
    let r = foldr (\p a -> let keys = findKeysRec area p bag (S.fromList[p]) iDistance in
                           let points2 = S.delete p points
                           in foldr (\(c2,d2,p2) a2 -> let kp = S.insert p2 points2 in
                                                       let kb = S.insert c2 bag in
                                                       let maybeMin m = Just (case m of
                                                                                Just v ->  (min v d2)
                                                                                _ ->  d2)
                                                       in M.alter maybeMin (kp,kb) a2) a keys) M.empty points
    in r

type Done = M.Map (S.Set Point, S.Set Char) Int
nextNode2 :: Area -> (Todo, Done) -> (Todo, Done)
nextNode2 area (todo,done) =
    let (nkey,nd) = head $ L.sortOn snd (M.toList todo) in
    let (np, bag) = nkey in
    let keys = findKeys2 area np bag nd in
    let ndone = M.insert nkey nd done in
    let ntodo = M.delete nkey todo
    in  M.foldrWithKey (\k d (td,dn) ->
        case (M.member k done, M.lookup k todo) of
            (False, Nothing) -> (M.insert k d td,dn)
            (False, Just tdd) -> (if d<tdd then M.insert k d td else td,dn)
            (_, _) -> (td,dn)) (ntodo, ndone) keys

addRobots :: M.Map Point Char -> M.Map Point Char
addRobots m = 
    let (x,y) = S.elemAt 0 $ findStarts2 m in
    let patch = [(-1,-1,'@'),( 0, -1,'#'),(1,-1,'@'),
                 (-1, 0,'#'),( 0,  0,'#'),(1, 0,'#'),
                 (-1, 1,'@'),( 0,  1,'#'),(1, 1,'@')]
    in foldr (\(dx,dy,c) a -> M.insert (x+dx,y+dy) c a) m patch

findStarts2 :: M.Map Point Char -> S.Set Point
findStarts2 m = S.fromList $ map fst  (M.toList (M.filterWithKey (\_ c -> c == '@') m))

shortestPath2 :: [String] -> Bool -> Int
shortestPath2 l part2 =
    let im = toMap (toInts l) in
    let m = if part2 then addRobots im else im in
    let start = findStarts2 m in
    let todo = findKeys2 m start S.empty 0 in
    let (_,done) = until (\(td,_) ->(length (M.elems td) == 0)) (nextNode2 m) (todo, M.fromList []) in
    let nbKeys = length $ filter isLower (M.elems m) in
    let (_,r) = head $ L.sortOn  (\(_,d) -> d) (filter (\((_,bag),_) -> length bag == nbKeys) (M.toList done))
    in r

run :: IO ()
run = do
    content <- readFile "src/day18_input.txt"
    print ("puzzle 1: " ++ show (shortestPath2 (lines (content)) False))
    getCurrentTime >>= print
    print ("puzzle 2: " ++ show (shortestPath2 (lines (content)) True))
    getCurrentTime >>= print
