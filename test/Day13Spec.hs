module Day13Spec (spec) where

import Test.Hspec

import Day13

spec :: Spec
spec = do
    describe "toTiles" $ do
        it "extract the tiles" $ do
            toTiles [1,2,3,6,5,4] `shouldBe` [((1,2),3),((6,5),4)]
