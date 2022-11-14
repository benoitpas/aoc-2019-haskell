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
 
spec :: Spec
spec = do
    describe "toMap" $ do
        it "creates a map from a list of characters" $ do
            toMap [46,65,10,10,66] `shouldBe` M.fromList [((0,0),'.'), ((0,2),'B'), ((1,0),'A')]

    describe "possibleDirections" $ do
        it "finds the possible directions at a given location" $ do
            possibleDirections ex1Map (5,1) S.empty (0,0) `shouldBe` [(1,0),(-1,0)]
        it "finds the possible directions at a given location with an empty bag" $ do
            possibleDirections ex1Map (6,1) S.empty (0,0) `shouldBe`  [(1,0),(-1,0)]
        it "finds the possible directions at a given location with a key in the bag" $ do
            possibleDirections ex1Map (6,1) (S.fromList ['a']) (0,0) `shouldBe` [(1,0),(-1,0)]

    describe "findKeys" $ do
        it "finds the available keys at a given location" $ do
            findKeys ex1Map (5,1) S.empty (0,0) 0 `shouldBe` [('a',2)]
        it "finds the available keys at a given location with an empty bag" $ do
            findKeys ex1Map (6,1) S.empty (0,0) 0 `shouldBe`  [('a',1)]
        it "finds the available keys at a given location with a key in the bag" $ do
            findKeys ex1Map (6,1) (S.fromList ['a']) (0,0) 0 `shouldBe` [('b',5)]

