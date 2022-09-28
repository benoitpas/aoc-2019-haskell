module Day7Spec (spec) where
import Test.Hspec

import Day7

program1 :: [Int]
program1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

program2 :: [Int]
program2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]

program3 :: [Int]
program3 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]

program21 :: [Int]
program21 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

phases21 :: [Int]
phases21 = [9,8,7,6,5]

program22 :: [Int]
program22 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,
    55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
    
phases22 :: [Int]
phases22 = [9,7,8,5,6]

spec :: Spec
spec = do

    describe "runAmps1" $ do
        it "runAmps1 for program 1" $ do
            runAmps1 program1 [4,3,2,1,0] `shouldBe` 43210
        it "runAmps1 for program 2" $ do
            runAmps1 program2 [0,1,2,3,4] `shouldBe` 54321
        it "runAmps1 for program 3" $ do
            runAmps1 program3 [1,0,4,3,2] `shouldBe` 65210

    describe "maxSignal" $ do
        it "maxSignal for program 1" $ do
            maxSignal1 program1 `shouldBe` 43210
        it "maxSignal for program 2" $ do
            maxSignal1 program2 `shouldBe` 54321
        it "maxSignal for program 3" $ do
            maxSignal1 program3 `shouldBe` 65210

    describe "runAmps2" $ do
        it "runAmps for program 21" $ do
            runAmps2 program21 phases21 `shouldBe` 139629729
        it "runAmps for program 22" $ do
            runAmps2 program22 phases22 `shouldBe` 18216

    describe "maxSignal" $ do
        it "maxSignal for program 21" $ do
            maxSignal2 program21 `shouldBe` 139629729
        it "maxSignal for program 22" $ do
            maxSignal2 program22 `shouldBe` 18216
