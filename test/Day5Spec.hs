module Day5Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess,prop)

import Day2 (list2array)
import Day5

instance Show State where
  show (State a b c d) = "{" ++ show a ++ "," ++ show b ++  "," ++ show c ++ "," ++ show d ++ "}"

instance Eq State where
  x == y = (memory x == memory y && ip x == ip y && input x == input y && output x == output y)

s1 = State {memory = list2array [3,0,4,0,99], ip= 0,input=42,output= Nothing}
s2 = State {memory = list2array [42,0,4,0,99], ip = 2, input= 42,output= Nothing}
s3 = State {memory = list2array [42,0,4,0,99], ip = 4, input= 42,output= Just 42}

spec :: Spec
spec = do

    describe "nextStep" $ do
        it "check reading from input" $ do
                (nextStep s1) `shouldBe` s2
        it "check writing to input" $ do
                (nextStep s2) `shouldBe` s3

    describe "runProgram" $ do
        prop "simple input/output test" $
            \i -> (runProgram "3,0,4,0,99" i) `shouldBe` Just (i::Int)
