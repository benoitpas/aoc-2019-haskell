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
        it "processes a jump if true" $ do
            (nextStep (State (list2array [1105,4,7]) 0 0 Nothing)) `shouldBe` (State (list2array [1105,4,7]) 7 0 Nothing)

    describe "allSteps"  $ do
        prop "comparison 1" $
            \i -> output (allSteps (State (list2array [3,9,8,9,10,9,4,9,99,-1,8]) 0 i Nothing))  `shouldBe` Just ((if i == 8 then 1 else 0) ::Int)
        prop "comparison 2" $
            \i -> output (allSteps (State (list2array [3,9,7,9,10,9,4,9,99,-1,8]) 0 i Nothing))  `shouldBe` Just ((if i < 8 then 1 else 0) ::Int)
        prop "comparison 3" $
            \i -> output (allSteps (State (list2array [3,3,1108,-1,8,3,4,3,99]) 0 i Nothing))  `shouldBe` Just ((if i == 8 then 1 else 0) ::Int)
        prop "comparison 4" $
            \i -> output (allSteps (State (list2array [3,3,1107,-1,8,3,4,3,99]) 0 i Nothing))  `shouldBe` Just ((if i < 8 then 1 else 0) ::Int)
        prop "comparison 5" $
            \i -> output (allSteps (State (list2array [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]) 0 i Nothing))  `shouldBe` Just (if i < 8 then 999 else  if i == 8 then 1000 else 1001)
        prop "jump 1" $
            \i -> output (allSteps (State (list2array [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) 0 i Nothing)) `shouldBe` Just (if i == 0 then 0 else 1)
        prop "jump 2" $
            \i -> output (allSteps (State (list2array [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) 0 i Nothing)) `shouldBe` Just (if i <= 0 then 0 else 1)
    describe "runProgram" $ do
        prop "simple input/output test" $
            \i -> (runProgram "3,0,4,0,99" i) `shouldBe` Just (i::Int)
