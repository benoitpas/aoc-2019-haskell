module Day2Spec (spec) where

import Test.Hspec
import Day2

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

    describe "allSteps"  $ do
        it "runs an addition" $ do
            (allSteps (list2array [1,0,0,0,99],0)) `shouldBe` (list2array [2,0,0,0,99], 4)
        it "runs a multiplication" $ do
            (allSteps (list2array [2,4,4,5,99,0],0)) `shouldBe` (list2array [2,4,4,5,99,9801], 4)
        it "runs a 2 operations program" $ do
            (allSteps (list2array [1,1,1,4,99,5,6,0,99],0)) `shouldBe` (list2array [30,1,1,4,2,5,6,0,99], 8)