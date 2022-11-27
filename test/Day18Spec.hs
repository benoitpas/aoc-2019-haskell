module Day18Spec (spec) where

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S

import Day3(Point)
import Day17(toMap, toInts)
import Day18

ex1 :: [String]
ex1 = 
   ["#########",
    "#b.A.@.a#",
    "#########"]

ex1Map :: M.Map Point Char
ex1Map = (toMap . toInts) ex1

ex2 :: [String]
ex2 =
   ["########################",
    "#f.D.E.e.C.b.A.@.a.B.c.#",
    "######################.#",
    "#d.....................#",
    "########################"]
 
ex3 :: [String]
ex3 = ["########################",
       "#...............b.C.D.f#",
       "#.######################",
       "#.....@.a.B.c.d.A.e.F.g#",
       "########################"]

ex4 :: [String]
ex4 = ["#################",
       "#i.G..c...e..H.p#",
       "########.########",
       "#j.A..b...f..D.o#",
       "########@########",
       "#k.E..a...g..B.n#",
       "########.########",
       "#l.F..d...h..C.m#",
       "#################"]

ex5 :: [String]
ex5 = ["########################",
       "#@..............ac.GI.b#",
       "###d#e#f################",
       "###A#B#C################",
       "###g#h#i################",
       "########################"]

ex21 :: [String]
ex21 = ["#######",
        "#a.#Cd#",
        "##...##",
        "##.@.##",
        "##...##",
        "#cB#Ab#",
        "#######"]

ex22 :: [String]
ex22 = ["###############",
        "#d.ABC.#.....a#",
        "######@#@######",
        "###############",
        "######@#@######",
        "#b.....#.....c#",
        "###############"]


ex23 :: [String]
ex23 = ["#############",
        "#DcBa.#.GhKl#",
        "#.###@#@#I###",
        "#e#d#####j#k#",
        "###C#@#@###J#",
        "#fEbA.#.FgHi#",
        "#############"]

ex24 :: [String]
ex24 = ["#############",
        "#g#f.D#..h#l#",
        "#F###e#E###.#",
        "#dCba@#@BcIJ#",
        "#############",
        "#nK.L@#@G...#",
        "#M###N#H###.#",
        "#o#m..#i#jk.#",
        "#############"]

spec :: Spec
spec = do
    describe "toMap" $ do
        it "creates a map from a list of characters" $ do
            toMap [46,65,10,10,66] `shouldBe` M.fromList [((0,0),'.'), ((0,2),'B'), ((1,0),'A')]

    describe "possibleDirections" $ do
        it "finds the possible directions at a given location" $ do
            possibleDirections ex1Map (5,1) S.empty (S.fromList [(5,1)]) `shouldBe` [(1,0),(-1,0)]
        it "finds the possible directions at a given location with an empty bag" $ do
            possibleDirections ex1Map (6,1) S.empty (S.fromList [(6,1)]) `shouldBe`  [(1,0),(-1,0)]
        it "finds the possible directions at a given location with a key in the bag" $ do
            possibleDirections ex1Map (6,1) (S.fromList ['a']) (S.fromList [(6,1)]) `shouldBe` [(1,0),(-1,0)]

    describe "findKeysRec" $ do
        it "finds the available keys at a given location" $ do
            findKeysRec ex1Map (5,1) S.empty (S.fromList [(5,1)]) 0 `shouldBe` [('a',2,(7,1))]
        it "finds the available keys at a given location with an empty bag" $ do
            findKeysRec ex1Map (6,1) S.empty (S.fromList [(6,1)]) 0 `shouldBe`  [('a',1, (7,1))]
        it "finds the available keys at a given location with a key in the bag" $ do
            findKeysRec ex1Map (6,1) (S.fromList ['a']) (S.fromList [(6,1)]) 0 `shouldBe` [('b',5, (1,1))]


    describe "shortestPath" $ do
        it "finds the shortest path from the start to collect all keys for example 1" $ do
            shortestPath ex1 False `shouldBe` 8
        it "finds the shortest ath from the start to collect all keys for example 2" $ do
            shortestPath ex2 False`shouldBe` 86
        it "finds the shortest path Path from the start to collect all keys for example 3" $ do
            shortestPath ex3 False `shouldBe` 132
        it "finds the shortest path from the start to collect all keys for example 4" $ do
            shortestPath ex4 False `shouldBe` 136
        it "finds the shortest path from the start to collect all keys for example 5" $ do
            shortestPath ex5 False `shouldBe` 81
        it "finds the shortest path from the start to collect all keys for example 21" $ do
            shortestPath ex21 False `shouldBe` 26
        it "finds the shortest path from the start to collect all keys for example 21 (puzzle 2 mode)" $ do
            shortestPath ex21 True `shouldBe` 8
        it "finds the shortest path from the start to collect all keys for example 22" $ do
            shortestPath ex22 False `shouldBe` 24
        it "finds the shortest path from the start to collect all keys for example 23" $ do
            shortestPath ex23 False `shouldBe` 32
        it "finds the shortest path from the start to collect all keys for example 24" $ do
            shortestPath ex24 False `shouldBe` 72
