module Day5Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Day5

spec :: Spec
spec = do

    describe "runProgram" $ do
        prop "simple input/output test" $
            \i -> (runProgram "3,0,4,0,99" i) `shouldBe` (i::Int)