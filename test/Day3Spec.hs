module Day3Spec (spec) where

import Test.Hspec
import Day3

w1 = "R8,U5,L5,D3"
w2 =  "U7,R6,D4,L4"

spec :: Spec
spec = do
    describe "points" $ do
        it "generates points from directions" $ do
            (points w2) `shouldBe` [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),
                (1,7),(2,7),(3,7),(4,7),(5,7),(6,7),(6,6),(6,5),(6,4),(6,3),(5,3),(4,3),(3,3),(2,3)]

    describe "intersection" $ do
        it "generates the points where the wires cross" $ do
            (intersection w1 w2) `shouldBe` [(0,0),(3,3),(6,5)]


    describe "distance" $ do
        it "find the distance to the closest intersection 1" $ do
            (distance w1 w2) `shouldBe` 6
    describe "distance" $ do
        it "find the distance to the closest intersection 2" $ do
            (distance "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83") `shouldBe` 159
    describe "distance" $ do
        it "find the distance to the closest intersection 2" $ do
            (distance "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") `shouldBe` 135

