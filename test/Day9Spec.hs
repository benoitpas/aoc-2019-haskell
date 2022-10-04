module Day9Spec (spec) where
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Data.Map

import Day5
import Day5Spec(stateFrom)

quince :: [Integer]
quince = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

s1 :: State
s1 = stateFrom quince 0

s2 :: State
s2 = State (fromList [(0,109),(1,1),(2,204),(3,-1),(4,1001),(5,100),(6,1),(7,100),(8,1008),(9,100),(10,16),(11,101),(12,1006),(13,101),(14,0),(15,99)]) 2 1 0 []

s3 :: State
s3 = s2 { ip = 4, output = [109]}

s4 :: State
s4 = s3 { memory = insert 100 1 (memory s3), ip = 8}
 
largeOutput :: State
largeOutput = stateFrom [1102,34915192,34915192,7,4,7,99,0] 0

spec :: Spec
spec = do

    describe "nextStep" $ do
        it "quince 1" $ do
            nextStep s1 `shouldBe` (s2, 9)
        it "quince 2" $ do
            nextStep s2 `shouldBe` (s3, 4)
        it "quince 3" $ do
            nextStep s3 `shouldBe` (s4, 1)
        prop "checks 203" $ do
            \i -> output (allSteps (State (fromList [(0,203),(1,0),(2,4),(3,1),(4,99)]) 0 1 i [])) `shouldBe`[i]

    describe "allSteps"  $ do
        it "runs a quice" $
            output (allSteps (stateFrom quince 0)) `shouldBe` quince
        it "outputs a large number" $
            output (allSteps largeOutput) `shouldBe` [1219070632396864]
