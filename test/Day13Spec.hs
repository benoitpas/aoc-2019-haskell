module Day13Spec (spec) where

import Test.Hspec

import Day13

spec :: Spec
spec = do
    describe "toTiles" $ do
        it "extract the tiles" $ do
            toTiles [1,2,3,6,5,4] `shouldBe` [(Paddle (1,2)),(Ball (6,5))]

    describe "paddleDirection" $ do
        it "makes it go right by default" $ do
            paddleDirection [] (1,1) [] `shouldBe` 1
        it "follow the ball" $ do
            paddleDirection [(10,5)] (1,1) [(20,19)] `shouldBe` -1
        it "follows the ball" $ do
            paddleDirection [(22,5)] (1,1) [(20,19)] `shouldBe` 1
        it "follow the ball" $ do
            paddleDirection [(10,5),(11,4)] (-1,1) [(20,19)] `shouldBe` -1
        it "follow the ball" $ do
            paddleDirection [(22,5),(21,7)] (1,1) [(20,19)] `shouldBe` 1