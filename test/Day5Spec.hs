module Day5Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess,prop)

import Day5

spec :: Spec
spec = do

    describe "runProgram" $ do
        modifyMaxSuccess (const 1) $ prop "simple input/output test" $
            \i -> (runProgram "3,0,4,0,99" i) `shouldBe`  Just (i::Int)
