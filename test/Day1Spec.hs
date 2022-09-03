module Day1Spec (spec) where

import Test.Hspec
import Day1

spec :: Spec
spec = do
  describe "fuel1" $ do
    it "computes the fuel for 12" $ do
      (fuel1 12)`shouldBe` 2
    it "computes the fuel for 14" $ do
      (fuel1 14)`shouldBe` 2