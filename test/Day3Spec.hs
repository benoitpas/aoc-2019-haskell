module Day3Spec (spec) where

import Test.Hspec
import Day3

w1 :: String
w1 = "R8,U5,L5,D3"

ws1 :: [Segment]
ws1 = [((0,0),(8,0)),((8,0),(8,5)),((8,5),(3,5)),((3,5),(3,2))]

w2 :: String
w2 =  "U7,R6,D4,L4"


ws2 :: [Segment]
ws2 = [((0,0),(0,7)),((0,7),(6,7)),((6,7),(6,3)),((6,3),(2,3))]

spec :: Spec
spec = do

    describe "segments" $ do
        it "generates segments from directions" $ do
            (segments w1) `shouldBe` ws1
        it "generates segments from directions" $ do
            (segments w2) `shouldBe` ws2

    describe "normalize" $ do
        it "puts the points in the right order" $ do
            (normalize ((8,5),(3,5))) `shouldBe` ((3,5),(8,5))
        it "puts the points in the right order" $ do
            (normalize ((6,7),(6,3))) `shouldBe` ((6,3),(6,7))

    describe "intersection" $ do
        it "generate the point where the 2 segments cross" $ do
            (intersection  (((-2,4),(6,4)),((1,2),(1,8)))) `shouldBe` [(1,4)]
        it "generate the point where the 2 segments cross" $ do
            (intersection  (((5,3),(5,8)),((3,6),(7,6)))) `shouldBe` [(5,6)]
        it "retuns an empty list when the 2 segments do not cross" $ do
            (intersection  (((0,0),(2,0)),((4,4),(4,3)))) `shouldBe` []

    describe "intersections" $ do
        it "generates the points where the wires cross" $ do
            (intersections ws1 ws2) `shouldBe` [(6,5),(3,3)]

    describe "distance" $ do
        it "find the distance to the closest intersection 1" $ do
            (distance w1 w2) `shouldBe` 6
        it "find the distance to the closest intersection 2" $ do
            (distance "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83") `shouldBe` 159
        it "find the distance to the closest intersection 3" $ do
            (distance "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") `shouldBe` 135

    describe "delay" $ do
        it "find the delay to an intersection 1" $ do
            (delay ws1 (3,3)) `shouldBe` 20
        it "find the delay to an intersection 2" $ do
            (delay ws2 (3,3)) `shouldBe` 20
        it "find the delay to an intersection 3" $ do
            (delay ws1 (6,5)) `shouldBe` 15
        it "find the delay to an intersection 4" $ do
            (delay ws2 (6,5)) `shouldBe` 15

    describe "shortestDelay" $ do
        it "find the shortest delay" $ do
            (shortestDelay w1 w2) `shouldBe` 30

