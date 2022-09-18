module Day5Spec (spec) where

import Test.Hspec

import Day5

spec :: Spec
spec = do

    describe "runProgram" $ do
        it "simple input/output test" $ do
            (runProgram "3,0,4,0,99" 12) `shouldBe` 12