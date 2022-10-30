module Day14Spec (spec) where

import Test.Hspec
import qualified Data.Map as M

import Day14

ex1 :: [String]
ex1 = ["10 ORE => 10 A",
    "1 ORE => 1 B",
    "7 A, 1 B => 1 C",
    "7 A, 1 C => 1 D",
    "7 A, 1 D => 1 E",
    "7 A, 1 E => 1 FUEL"]

spec :: Spec
spec = do
    describe "extractPair" $ do
        it "extracts one pair from the list" $ do
            extractPair ([],["7","A","1","E"]) `shouldBe` ([("A",7)],["1","E"])
    describe "parseLine" $ do
        it "extracts the field from one line of the input" $ do
            parseLine "7 A, 1 E => 1 FUEL" `shouldBe` Just (("FUEL",1),[("E",1),("A",7)])
    describe "getGraph" $ do
        it "extracts one line of a graph" $ do
            M.toList (getGraph ["10 ORE => 10 A"]) `shouldBe` [("A",(10,[("ORE",10)]))]
        it "extracts a more complicated line of a graph" $ do
            M.toList (getGraph ["7 A, 1 D => 1 E"]) `shouldBe` [("E",(1,[("D",1),("A",7)]))]
        it "extracts a full graph" $ do
            M.toList (getGraph ex1) `shouldBe` [("A",(10,[("ORE",10)])),
                ("B",(1,[("ORE",1)])),
                ("C",(1,[("B",1),("A",7)])),
                ("D",(1,[("C",1),("A",7)])),
                ("E",(1,[("D",1),("A",7)])),
                ("FUEL",(1,[("E",1),("A",7)]))]
