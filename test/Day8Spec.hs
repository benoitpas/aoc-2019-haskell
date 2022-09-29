module Day8Spec (spec) where
import Test.Hspec

import Day8

imageData :: String
imageData = "0222112222120000"

imageData2 :: [[Int]]
imageData2 = [[0,2,2,2],[1,1,2,2],[2,2,1,2],[0,0,0,0]]

spec :: Spec
spec = do

    describe "split" $ do
        it "splits a collection to collection of collections" $ do
            split 5 imageData `shouldBe` ["02221","12222","12000","0"]

    describe "toImages" $ do
        it "converts imageData to images" $ do
            toImages 2 2 imageData `shouldBe` [[0,2,2,2],[1,1,2,2],[2,2,1,2],[0,0,0,0]]

    describe "countDigits" $ do
        it "count zeros in an image" $ do
            countDigits 0 [0,1,2,3,0,1]`shouldBe` 2

    describe "toImage" $ do
        it "maps all the layers to an image" $ do
            toImage imageData2`shouldBe` [0,1,1,0]
