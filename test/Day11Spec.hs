module Day11Spec (spec) where

import Test.Hspec

import qualified Data.Map as M

import Day11
import Day3 (Point)

m :: M.Map Point Int
m = M.fromList [((0,0),1),((1,1),1),((-1,1),1)]

spec :: Spec
spec = do
    describe "nextDirection" $ do
        it "nextDirection pointing up, going left" $ do
            nextDirection (0,-1) 0 `shouldBe` (-1, 0)
        it "nextDirection pointing up, going right" $ do
            nextDirection (0,-1) 1 `shouldBe` (1, 0)
        it "nextDirection pointing left, going left" $ do
            nextDirection (-1, 0) 0 `shouldBe` (0, 1)
        it "nextDirection pointing left, going right" $ do
            nextDirection (-1, 0) 1 `shouldBe` (0, -1)

    describe "mapToGrid" $ do
        it "generate a grid from a simple map" $ do
            mapToGrid m `shouldBe` [" # ","# #"]

