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

spec :: Spec
spec = do

    describe "loadNodes" $ do
        it "loadNodes for example 1" $ do
                (loadNodes ex1) `shouldBe` fromList [("K",["L"]),("J",["K"]),("G",["H"]),("E",["F","J"]),("D",["E","I"]),
                    ("C",["D"]),("B",["C","G"]),("COM",["B"])]
                
    describe "nbPaths" $ do
        it "nbPaths for example 1" $ do
                (nbPaths ex1) `shouldBe` 42