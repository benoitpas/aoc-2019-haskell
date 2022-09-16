module Day4Spec (spec) where

import Test.Hspec

import Day4

spec :: Spec
spec = do

    describe "countDigits" $ do
        it "count the digits" $ do
            (countDigits [1,1,2,3,3]) `shouldBe` [2,1,2,0,0,0,0,0,0]

    describe "atLeastOnePair" $ do
        it "is true when there is at least one pair " $ do
            (atLeastOnePair [1,1,1,1,1]) `shouldBe` True
        it "is false when there is no pair " $ do
            (atLeastOnePair [1,2,3,4,5]) `shouldBe` False

    describe "onePair" $ do
        it "is true when there is only one pair " $ do
            (onePair [1,1,1,1,1]) `shouldBe` False
        it "is true when there is only one pair " $ do
            (onePair [1,1,1,2,2]) `shouldBe` True
        it "is false when there is no pair " $ do
            (onePair [1,2,3,4,5]) `shouldBe` False
