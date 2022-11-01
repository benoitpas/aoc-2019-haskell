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

ex2 :: [String]
ex2  = ["157 ORE => 5 NZVS",
    "165 ORE => 6 DCFZ",
    "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL",
    "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ",
    "179 ORE => 7 PSHF",
    "177 ORE => 5 HKGWZ",
    "7 DCFZ, 7 PSHF => 2 XJWVT",
    "165 ORE => 2 GPVTF",
    "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]

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
            getGraph ["10 ORE => 10 A"] `shouldBe` M.fromList [("A",(10,[("ORE",10)]))]
        it "extracts a more complicated line of a graph" $ do
            getGraph ["7 A, 1 D => 1 E"] `shouldBe` M.fromList [("E",(1,[("D",1),("A",7)]))]
        it "extracts a full graph" $ do
            getGraph ex1 `shouldBe` M.fromList [("A",(10,[("ORE",10)])),
                ("B",(1,[("ORE",1)])),
                ("C",(1,[("B",1),("A",7)])),
                ("D",(1,[("C",1),("A",7)])),
                ("E",(1,[("D",1),("A",7)])),
                ("FUEL",(1,[("E",1),("A",7)]))]

        it "computes the number of ORE for example 1" $ do
            calculateOre (getGraph ex1) 1 `shouldBe` 31

        it "computes the number of ORE for example 2" $ do
            calculateOre (getGraph ex2) 1 `shouldBe` 13312

        it "computes how much fuel for 1000000000000 ore for example 2" $ do
            findFuel (getGraph ex2) `shouldBe` 82892753

