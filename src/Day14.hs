module Day14
    (
        extractPair, parseLine,
        getGraph, calculateOre,
        findFuel,
        run
    ) where

import qualified Data.List as L
import qualified Data.Map as M

extractPair :: Read b => ([(String, b)], [String]) -> ([(String, b)], [String])
extractPair (pairs, list) = case list of
                            a:b:r -> ((b, read a):pairs, r)
                            _ -> (pairs,[])

parseLine :: (Read a, Integral a) => String -> Maybe ((String, a),[(String, a)])
parseLine s = 
    let w = words (map (\c -> if c == ',' then ' ' else c) s) in
    let k = last w
        q = (read (last (init w))) in
    let list = take (length w - 3) w in
    let pairs = until (\(_,l) -> length l == 0) extractPair ([],list)
    in Just ((k, q), fst pairs)

getGraph :: (Read a, Integral a) => [String] -> M.Map String (a, [(String, a)])
getGraph strings = foldr (\s -> \m -> 
    case parseLine s of
        Just ((k,v),pairs) -> M.insert k (v, pairs) m
        _-> m) M.empty strings

calculateOre :: (Read a, Integral a) => M.Map String (a, [(String, a)]) -> a -> a
calculateOre graph fuel =
    let getOre label req stockMap nbOre = case label of
            "ORE" -> (stockMap, req + nbOre)
            _ -> let (output, pairs) = graph M.! label in
                 let stock = M.findWithDefault 0 label stockMap in
                 let (coef, nStock) =
                        if req <= stock then
                            (0, stock - req)
                        else
                            let coef = (req - stock + output - 1) `div` output
                            in (coef, output * coef - req + stock)
                 in foldr (\(l,o) -> \(s,r) -> getOre l (o * coef) s r) (M.insert label nStock stockMap, nbOre) pairs

    in snd $ getOre "FUEL" fuel M.empty 0

findFuel :: (Read a, Integral a) => M.Map String (a, [(String, a)]) -> a
findFuel graph = 
    let ore = 1000000000000 in
    let ore1Fuel = calculateOre graph 1 in
    let iMinFuel = ore `div` ore1Fuel in
    let iMaxFuel = iMinFuel * 2 in
    let next (minFuel, maxFuel) =
            let avFuel = (minFuel + maxFuel) `div` 2 in
            let avOre = calculateOre graph avFuel
            in if (avOre > ore) then (minFuel, avFuel) else (avFuel, maxFuel)
    in fst $ until (\(minFuel, maxFuel) -> maxFuel == minFuel + 1) next (iMinFuel, iMaxFuel)

run :: IO ()
run = do
    content <- readFile "src/day14_input.txt"
    let g = getGraph $ lines content
    print ("puzzle 1: " ++ show (calculateOre g 1))
    print ("puzzle 2: " ++ show (findFuel g ))