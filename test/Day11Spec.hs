module Day11Spec (spec) where

import Test.Hspec

import Day11

spec :: Spec
spec = do
  describe "nextDirection" $ do
    it "nextDirection pointing up, going left" $ do
      nextDirection (0,-1) 0 `shouldBe` (-1, 0)
    it "nextDirection pointing up, going right" $ do
      nextDirection (0,-1) 1 `shouldBe` (1, 0)
    it "nextDirection pointing left, going left" $ do
      nextDirection (-1, 0) 0 `shouldBe` (0, 1)
    it "nextDirection pointing left, going right" $ do
      nextDirection (-1, 0) 1 `shouldBe` (0, -1)