module Day5Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Day2 (list2array)
import Day5

s1 :: State
s1 = State {memory = list2array [3,0,4,0,99], ip = 0, input = 42, output = Nothing}

s2 :: State
s2 = State {memory = list2array [42,0,4,0,99], ip = 2, input = 42, output = Nothing}

s3 :: State
s3 = State {memory = list2array [42,0,4,0,99], ip = 4, input = 42, output = Just 42}

spec :: Spec
spec = do

    describe "nextStep" $ do
        it "check reading from input" $ do
                (nextStep s1) `shouldBe` s2
        it "check writing to input" $ do
                (nextStep s2) `shouldBe` s3
        it "ends the program" $ do
            (nextStep (State (list2array [99]) 0 0 Nothing)) `shouldBe` (State (list2array [99]) (-1) 0 Nothing)
        it "processes an addition" $ do
            (nextStep (State (list2array [1,0,0,0]) 0 0 Nothing)) `shouldBe` (State (list2array [2,0,0,0]) 4 0 Nothing)
        it "processes an addition with immediate access" $ do
            (nextStep (State (list2array [1101,100,-1,4,0]) 0 0 Nothing)) `shouldBe` (State (list2array [1101,100,-1,4,99]) 4 0 Nothing)
        it "processes a multiplication" $ do
            (nextStep (State (list2array [2,3,0,3,99]) 0 0 Nothing)) `shouldBe` (State (list2array [2,3,0,6,99]) 4 0 Nothing)
        it "processes a multiplication with immediate access" $ do
            (nextStep (State (list2array [1002,4,3,4,33]) 0 0 Nothing)) `shouldBe` (State (list2array [1002,4,3,4,99]) 4 0 Nothing)


    describe "runProgram" $ do
        prop "simple input/output test" $
            \i -> (runProgram "3,0,4,0,99" i) `shouldBe` Just (i::Int)
