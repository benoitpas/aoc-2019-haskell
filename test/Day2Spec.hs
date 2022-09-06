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
