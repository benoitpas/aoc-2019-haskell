module Day14
    (
        extractPair, parseLine,
        getGraph,
        run
    ) where

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

getGraph ::  (Read a, Integral a) => [String] -> M.Map String (a, [(String, a)])
getGraph strings = foldr (\s -> \m -> 
    case parseLine s of
        Just ((k,v),pairs) -> M.insert k (v, pairs) m
        _-> m) M.empty strings

run :: IO ()
run = do
    content <- readFile "src/day14_input.txt"
    print (show (getGraph (lines (content))))