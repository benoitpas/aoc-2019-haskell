module Day16Spec (spec) where

import Test.Hspec
import qualified Data.Map as M

import Day16

spec :: Spec
spec = do
    describe "pattern" $ do
        it "generates the pattern for a given index" $ do
            take 10 (pattern 2) `shouldBe` [0,1,1,0,0,-1,-1,0,0,1]

    describe "onePhase" $ do
        it "generates the output signal for an input signal" $ do
            onePhase [1..8] `shouldBe` [4,8,2,2,6,1,5,8]

    describe "output" $ do
        it "generates the output signal for an input signal after 4 phases" $ do
            output "12345678" 4 `shouldBe` "01029498"
        it "generates the output signal for an input signal after 100 phases" $ do
            take 8 (output "80871224585914546619083218645595" 100) `shouldBe` "24176176"
        it "generates the output signal for an input signal after 100 phases" $ do
            take 8 (output "19617804207202209144916044189917" 100) `shouldBe` "73745418"
        it "generates the output signal for an input signal after 100 phases" $ do
            take 8 (output "69317163492948606335995924319873" 100) `shouldBe` "52432133"
