module Day7Spec (spec) where
import Test.Hspec

import Day7

program1 :: [Int]
program1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

program2 :: [Int]
program2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]

program3 :: [Int]
program3 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]

spec :: Spec
spec = do

    describe "runAmps" $ do
        it "runAmps for program 1" $ do
            runAmps program1 [4,3,2,1,0] `shouldBe` 43210
        it "runAmps for program 2" $ do
            runAmps program2 [0,1,2,3,4] `shouldBe` 54321
        it "runAmps for program 3" $ do
            runAmps program3 [1,0,4,3,2] `shouldBe` 65210

    describe "maxSignal" $ do
        it "maxSignal for program 1" $ do
            maxSignal program1 `shouldBe` 43210
        it "maxSignal for program 2" $ do
            maxSignal program2 `shouldBe` 54321
        it "maxSignal for program 3" $ do
            maxSignal program3 `shouldBe` 65210