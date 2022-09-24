module Day6
    (
        loadNodes,nbPaths, toCom,
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

nbPaths :: [String] -> Int
nbPaths orbitals = 
    let m = loadNodes orbitals in
    let pair key = case (Data.HashMap.lookup key m) of
                        Just l -> let pairs = Prelude.map pair l in foldr (\p -> \acc -> (fst acc + fst p + snd p + 1 , snd acc + snd p +1)) (0,0) pairs
                        _ -> (0,0) 
    in fst (pair "COM")

-- reverses the orientated graph stored in the map
--reverse :: Map String [String] -> Map String [String]
--reverse grap = 

toCom :: Map String [String] -> String -> [String]
toCom graph start = case start of
                        "COM" -> []
                        _ -> case Data.HashMap.toList (Data.HashMap.filter (\v -> any (== start) v) graph) of
                            (k,_):_ -> toCom graph k ++ [k]
                            _ -> []

run :: IO ()
run = do
    content <- readFile "src/day6_input.txt"
    let graph = (lines content)
    print ("puzzle 1: " ++ show (nbPaths graph))

