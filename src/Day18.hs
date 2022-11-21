module Day18
    (
        possibleDirections,
        findKeys,
        buildTree,
        strBuildTree,
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
    in c == '.' || c == '@' || isLower c || (toLower c) `L.elem` bag

possibleDirections area (sx,sy) bag prevLocations =
    let directions = [(0,1), (1,0), (0,-1),(-1,0)]  in
    directions  >>= (\(pdx, pdy) -> let (nx,ny)= (sx+pdx,sy+pdy)
                                    in if isFree area bag (nx,ny) && S.notMember (nx,ny) prevLocations then [(pdx,pdy)] else [])

findKeys area (sx,sy) bag prevLocations iDistance =
    let pd = possibleDirections area (sx,sy) bag prevLocations in
    let nDistance = iDistance + 1
    in pd >>= (\(pdx, pdy) -> 
        let (nsx,nsy) = (sx+pdx, sy+pdy) in
        let c = checkLocation (nsx,nsy) area
        in case (isLower c, c `L.elem` bag) of
                    (True, False) -> [(c, nDistance, (nsx,nsy))]
                    _ -> findKeys area (nsx,nsy) bag (S.insert (nsx,nsy) prevLocations) nDistance)

findKeys2 :: M.Map (Int, Int) Char -> (Int, Int) -> [ Char] -> Int -> M.Map Char (Int, (Int, Int))
findKeys2 area (sx,sy) bag iDistance =
    let r = findKeys area (sx,sy) bag (S.fromList[(sx,sy)]) iDistance
    in foldr (\(c,d,p) a -> let nd = case (M.lookup c a) of
                                                    Just (ad,ap) -> min d ad
                                                    _ -> d in M.insert c (nd,p) a) M.empty r

nextKey m status = 
    let r = status >>= (\(keys,(d,p)) -> let keys2 = findKeys2 m p keys d in map (\(c,(d,p)) -> (keys ++ [c], (d,p) )) (M.toList keys2))
    in r
--    in take 1000 $ L.sortOn (\(keys,(d,p)) -> d) r
--    in M.toList $ foldr (\(keys,(d,p)) a -> case (M.lookup keys a) of 
--                                                Just (ad,ap) -> if d < ad then M.insert keys (d,p) a else a
--                                                _ -> M.insert keys (d,p) a ) M.empty r

shortestPath l =
    let m = toMap (toInts l) in
    let start = fst $ head $ M.toList (M.filterWithKey (\_ c -> c == '@') m) in
    let nbKeys = length $ filter isLower (M.elems m) in
    let lstates = last $ take (nbKeys + 1) (iterate (nextKey m) [([], (0,start))]) in
    let (_,(d,_)) = head $ L.sortOn (\(_,(d,_)) -> d) lstates
    in d
--    in (iterate (nextKey m) [([], (0,start))])

 --   in  [minimum $ map (\(_,(d,_)) -> d) s]
--    let keys1 = findKeys2 m start S.empty 0 in
--    let s1 = map (\(c,(d,p)) -> (S.fromList [c],(d,p))) keys1 in
--    let s2 = s1 >>= (\(keys,(d,p)) -> let keys2 = findKeys2 m p keys d in map (\(c,(d,p)) -> (S.insert c keys, (d,p) )) keys2)
--    in s2 >>= (\(keys,(d,p)) -> let keys2 = findKeys2 m p keys d in map (\(c,(d,p)) -> (S.insert c keys, (d,p) )) keys2)

buildTree m (sx,sy) iBag iDistance iTree =
    if M.member iBag iTree then
        iTree
    else
        let keys = M.toList $ findKeys2 m (sx,sy) iBag iDistance in
        let tree = M.insert iBag keys iTree
        in foldr (\(c,(d,p)) a -> buildTree m p (iBag ++ [c]) d a) tree keys

strBuildTree l =
    let m = toMap (toInts l) in
    let start = fst $ head $ M.toList (M.filterWithKey (\_ c -> c == '@') m) 
    in buildTree m start "" 0 M.empty

shortestPath2 l =
    let m = toMap (toInts l) in
    let start = fst $ head $ M.toList (M.filterWithKey (\_ c -> c == '@') m) in
    let keys = findKeys2 m start [] 0 in
    let todo = M.map (\(d,p) -> (d,p,[])) keys
    in  take 10 (iterate (nextNode m) (todo, M.fromList []))

findNextNode :: M.Map Char (Int,(Int, Int), [Char]) -> (Int, Char, (Int,Int), [Char])
findNextNode todo =
    let l = L.sortOn (\(c,(d,p, prev)) -> d) (M.toList todo) in
    let (c,(d,p, prev)) = head l
    in (d,c,p, prev)

nextNode :: M.Map (Int, Int) Char -> (M.Map Char (Int,(Int, Int), [Char]), M.Map Char (Int, [Char])) 
    -> (M.Map Char (Int,(Int, Int), [Char]), M.Map Char (Int, [Char]))
nextNode area (todo,done) =
    let (nd,nc,np,nprev) = findNextNode todo in
    let keys = findKeys2 area np nprev nd in
    let ndone = M.insert nc (nd, nprev) done in
    let ntodo = M.delete nc todo in
    let prev = (nc:nprev)
    in  M.foldrWithKey (\c (d,p) (td,dn) ->
        case (M.member c done, M.lookup c todo) of
            (False, Nothing) -> (M.insert c (d,p, prev) td,dn)
            (False, Just (tdd, tdp, tdprev)) -> (if d<tdd then M.insert c (d,p, prev) td else td,dn)
            (_, _) -> (td,dn)) (ntodo, ndone) keys
--    in  M.foldrWithKey (\c (d,p) (td,dn) -> if M.member c done then 
--            (td,dn)
--        else -- if in todo, replace entry if distance is less
--            (td,dn)) (todo,done) keys
    


run :: IO ()
run = do
    content <- readFile "src/day18_input.txt"
    putStrLn content
    print (show (shortestPath2 (lines (content))))