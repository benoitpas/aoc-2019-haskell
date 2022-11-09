module Day17Spec (spec) where

import Test.Hspec
import qualified Data.Map as M

import Day17

import Data.Char

grid :: [String]
grid = 
   ["..#..........",
    "..#..........",
    "#######...###",
    "#.#...#...#.#",
    "#############",
    "..#...#...#..",
    "..#####...^.."]

mapedGrid = grid >>= (\s -> map (toInteger . ord) s ++ [10])

mapedGrid :: [Integer]

spec :: Spec
spec = do
    describe "toMap" $ do
        it "creates a map from a list of characters" $ do
            toMap [46,65,10,10,66] `shouldBe` M.fromList [((0,0),'.'), ((0,2),'B'), ((1,0),'A')]

    describe "sumAligments" $ do
        it "adds the coordinates of the intersection of the scaffoldings" $ do
            sumAligments (toMap mapedGrid) `shouldBe` 76

