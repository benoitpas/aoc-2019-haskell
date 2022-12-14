module Day6Spec (spec) where
import Test.Hspec

import Data.HashMap

import Day6

ex1 :: [String]
ex1 = ["COM)B",
    "B)C",
    "C)D",
    "D)E",
    "E)F",
    "B)G",
    "G)H",
    "D)I",
    "E)J",
    "J)K",
    "K)L"]

mapEx1 :: Map String [String]
mapEx1 = loadNodes ex1

mapEx2 :: Map String [String]
mapEx2 = Data.HashMap.insert "I" ["SAN"] (Data.HashMap.insert "K" ["L","YOU"] mapEx1)

spec :: Spec
spec = do

    describe "loadNodes" $ do
        it "loadNodes for example 1" $ do
            mapEx1 `shouldBe` fromList [("K",["L"]),("J",["K"]),("G",["H"]),("E",["F","J"]),("D",["E","I"]),
                    ("C",["D"]),("B",["C","G"]),("COM",["B"])]
                
    describe "nbPaths" $ do
        it "nbPaths for example 1" $ do
            (nbPaths mapEx1) `shouldBe` 42

    describe "toCom" $ do
        it "does zero hop" $ do
            toCom mapEx1 "COM" `shouldBe` []

        it "does one hop" $ do
            (toCom mapEx1 "B") `shouldBe` ["COM"]

        it "does two hops" $ do
            (toCom mapEx1 "C") `shouldBe` ["COM","B"]

        it "does more hops" $ do
            (toCom mapEx1 "I") `shouldBe` ["COM","B","C","D"]

    describe "distance" $ do
        it "find the orbitals" $ do
            (distance mapEx2) `shouldBe` 4

