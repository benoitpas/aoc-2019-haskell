module Day2Spec (spec) where

import Test.Hspec
import Day2

spec :: Spec
spec = do
  describe "wordsWhen" $ do
    it "splits a string" $ do
      (wordsWhen (==' ') "Salut les amis") `shouldBe` ["Salut","les","amis"]
