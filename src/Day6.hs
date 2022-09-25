module Day6
    (
        loadNodes,nbPaths, toCom, distance,
        run
    ) where

import Data.HashMap

import Day2 (wordsWhen)

loadNodes :: [String] -> Map String [String]
loadNodes pairs = foldr (\pair -> \m -> 
    let r = case wordsWhen (==')') pair of
                [k,v] -> alter (\mlist -> case mlist of
                    Just l ->  Just (v:l)
                    Nothing -> Just [v]) k m
                _ -> m
    in r) empty pairs

nbPaths :: Map String [String] -> Int
nbPaths m = 
    let pair key = case (Data.HashMap.lookup key m) of
                        Just l -> let pairs = Prelude.map pair l in foldr (\p -> \acc -> (fst acc + fst p + snd p + 1 , snd acc + snd p +1)) (0,0) pairs
                        _ -> (0,0) 
    in fst (pair "COM")

toCom :: Map String [String] -> String -> [String]
toCom graph start = case start of
                        "COM" -> []
                        _ -> case Data.HashMap.toList (Data.HashMap.filter (\v -> any (== start) v) graph) of
                            (k,_):_ -> toCom graph k ++ [k]
                            _ -> []

distance :: Map String [String] -> Int
distance graph = 
    let youPath = toCom graph "YOU" 
        canPath = toCom graph "SAN" in 
    let commonPairs = takeWhile (\(x,y) -> x == y) (youPath `zip` canPath)
    in length youPath + length canPath - 2 * length commonPairs 

run :: IO ()
run = do
    content <- readFile "src/day6_input.txt"
    let graph = loadNodes (lines content)
    print ("puzzle 1: " ++ show (nbPaths graph))
    print ("puzzle 2: " ++ show (distance graph))
 