module Day2Spec (spec) where

import Test.Hspec
import Data.Array
import Day2

p1 :: Array Int Int
p1 = str2array "1,9,10,3,2,3,11,0,99,30,40,50"

spec :: Spec
spec = do
    describe "wordsWhen" $ do
        it "splits a string" $ do
            (wordsWhen (==' ') "Salut les amis") `shouldBe` ["Salut","les","amis"]

    describe "nextStep" $ do
        it "ends the program" $ do
            (nextStep (list2array [99], 0)) `shouldBe` (list2array [99], -1)
        it "processes an addition" $ do
            (nextStep (list2array [1,0,0,0], 0)) `shouldBe` (list2array [2,0,0,0], 4)
        it "processes a multiplication" $ do
            (nextStep (list2array [2,3,0,3,99], 0)) `shouldBe` (list2array [2,3,0,6,99], 4)
