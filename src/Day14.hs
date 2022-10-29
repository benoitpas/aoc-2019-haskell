module Day14
    (
        extractPair, parseLine,
        getGraph,
        run
    ) where

import qualified Data.List as L
import qualified Data.Map as M

extractPair (pairs, list) = case list of
                            a:b:r -> ((b, read a):pairs, r)
                            _ -> (pairs,[])

parseLine :: String -> Maybe ((String, Integer),[(String, Integer)])
parseLine s = 
    let w = words (map (\c -> if c == ',' then ' ' else c) s) in
    let k = last w
        q = (read (last (init w))) in
    let list = take (length w - 3) w in
    let pairs = until (\(_,l) -> length l == 0) extractPair ([],list)
    in Just ((k, q), fst pairs)

getGraph :: Integral a => String -> M.Map String (a, [(String, a)])
getGraph s = M.empty

run :: IO ()
run = do
    content <- readFile "src/day14_input.txt"
    print (show (length (lines content)))