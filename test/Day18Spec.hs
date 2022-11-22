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

ex2 =
   ["########################",
    "#f.D.E.e.C.b.A.@.a.B.c.#",
    "######################.#",
    "#d.....................#",
    "########################"]
 
ex3 = ["########################",
       "#...............b.C.D.f#",
       "#.######################",
       "#.....@.a.B.c.d.A.e.F.g#",
       "########################"]

ex4 = ["#################",
       "#i.G..c...e..H.p#",
       "########.########",
       "#j.A..b...f..D.o#",
       "########@########",
       "#k.E..a...g..B.n#",
       "########.########",
       "#l.F..d...h..C.m#",
       "#################"]

ex5 = ["########################",
       "#@..............ac.GI.b#",
       "###d#e#f################",
       "###A#B#C################",
       "###g#h#i################",
       "########################"]

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

    describe "findKeys" $ do
        it "finds the available keys at a given location" $ do
            findKeys ex1Map (5,1) S.empty (S.fromList [(5,1)]) 0 `shouldBe` [('a',2,(7,1))]
        it "finds the available keys at a given location with an empty bag" $ do
            findKeys ex1Map (6,1) S.empty (S.fromList [(6,1)]) 0 `shouldBe`  [('a',1, (7,1))]
        it "finds the available keys at a given location with a key in the bag" $ do
            findKeys ex1Map (6,1) (S.fromList ['a']) (S.fromList [(6,1)]) 0 `shouldBe` [('b',5, (1,1))]


    describe "shortestPath2" $ do
        it "finds the shortestPath from the start to collect all keys for example 1" $ do
            shortestPath2 ex1 `shouldBe` 8
        it "finds the shortestPath from the start to collect all keys for example 2" $ do
            shortestPath2 ex2 `shouldBe` 86
        it "finds the shortestPath from the start to collect all keys for example 3" $ do
            shortestPath2 ex3 `shouldBe` 132
--        it "finds the shortestPath from the start to collect all keys for example 4" $ do
--            shortestPath2 ex4 `shouldBe`  []
        it "finds the shortestPath from the start to collect all keys for example 5" $ do
            shortestPath2 ex5 `shouldBe` 81
