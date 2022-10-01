module Day5Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Day2 (list2array, Memory)
import Day5

stateFrom :: [Integer] -> Integer -> State
stateFrom m i = State (list2array m) 0 0 i []

s1 :: State
s1 = stateFrom [3,0,4,0,99] 42

s2 :: State
s2 = s1 { memory = list2array [42,0,4,0,99], ip = 2 }

s3 :: State
s3 = s2 { ip = 4, output = [42] }

quince :: [Integer]
quince = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

spec :: Spec
spec = do

    describe "nextStep" $ do
        it "check reading from input" $ do
            nextStep s1 `shouldBe` (s2, 3)
        it "check writing to input" $ do
            nextStep s2 `shouldBe` (s3, 4)
        it "ends the program" $ do
            nextStep (stateFrom [99] 0) `shouldBe` (State (list2array [99]) (-1) 0 0 [], 99)
        it "processes an addition" $ do
            nextStep (stateFrom [1,0,0,0] 0) `shouldBe` (State (list2array [2,0,0,0]) 4 0 0 [], 1)
        it "processes an addition with immediate access" $ do
            nextStep (stateFrom [1101,100,-1,4,0] 0) `shouldBe` (State (list2array [1101,100,-1,4,99]) 4 0 0 [], 1)
        it "processes a multiplication" $ do
            nextStep (stateFrom [2,3,0,3,99] 0) `shouldBe` (State (list2array [2,3,0,6,99]) 4 0 0 [], 2)
        it "processes a multiplication with immediate access" $ do
            nextStep (stateFrom [1002,4,3,4,33] 0) `shouldBe` (State (list2array [1002,4,3,4,99]) 4 0 0 [], 2)
        it "processes a jump if true" $ do
            nextStep (stateFrom [1105,4,7] 0) `shouldBe` (State (list2array [1105,4,7]) 7 0 0 [], 5)
        it "updates the relative base" $ do
            nextStep (State (list2array [109,19]) 0 2000 0 []) { base = 2000} `shouldBe` (State (list2array [109,19]) 2 2019 0 [], 9)

    describe "allSteps"  $ do
        prop "comparison 1" $
            \i -> output (allSteps (stateFrom [3,9,8,9,10,9,4,9,99,-1,8] i))  `shouldBe` [if i == 8 then 1 else 0]
        prop "comparison 2" $
            \i -> output (allSteps (stateFrom [3,9,7,9,10,9,4,9,99,-1,8] i))  `shouldBe` [if i < 8 then 1 else 0]
        prop "comparison 3" $
            \i -> output (allSteps (stateFrom [3,3,1108,-1,8,3,4,3,99] i))  `shouldBe` [if i == 8 then 1 else 0]
        prop "comparison 4" $
            \i -> output (allSteps (stateFrom [3,3,1107,-1,8,3,4,3,99] i))  `shouldBe` [if i < 8 then 1 else 0]
        prop "comparison 5" $
            \i -> output (allSteps (stateFrom [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,
                4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] i))  `shouldBe`  
                    [if i < 8 then 999 else  if i == 8 then 1000 else 1001]
        prop "jump 1" $
            \i -> output (allSteps (stateFrom [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] i)) `shouldBe` [if i == 0 then 0 else 1]
        prop "jump 2" $
            \i -> output (allSteps (stateFrom [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] i)) `shouldBe` [if i <= 0 then 0 else 1]
--        it "runs  a quice" $
--            output (allSteps (stateFrom quince 0)) `shouldBe` quince


    describe "runProgram" $ do
        prop "simple input/output test" $
            \i -> (runProgram "3,0,4,0,99" i) `shouldBe` [i::Integer]
