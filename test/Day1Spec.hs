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
    it "computes the fuel for 1969" $ do
      (fuel1 1969)`shouldBe` 654
    it "computes the fuel for 100756" $ do
      (fuel1 100756)`shouldBe` 33583

  describe "fuel2" $ do
    it "computes the fuel for 12" $ do
      (fuel2 12)`shouldBe` 2
    it "computes the fuel for 14" $ do
      (fuel2 14)`shouldBe` 2
    it "computes the fuel for 1969" $ do
      (fuel2 1969)`shouldBe` 966
    it "computes the fuel for 100756" $ do
      (fuel2 100756)`shouldBe` 50346