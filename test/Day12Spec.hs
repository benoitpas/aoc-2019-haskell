module Day12Spec (spec) where

import Test.Hspec

import Day12

-- <x=-1, y=0, z=2>
-- <x=2, y=-10, z=-7>
-- <x=4, y=-8, z=8>
-- <x=3, y=5, z=-1>
ex1 :: [[Int]]
ex1 = [[-1,  0,  2],
       [ 2,-10, -7],
       [ 4, -8,  8],
       [ 3,  5, -1]]

v1 :: [[Int]]
v1 = [[0,0,0],
      [0,0,0],
      [0,0,0],
      [0,0,0]]

ex1_step1 :: [[Int]]
ex1_step1 = [[ 2,-1, 1],
       [ 3,-7,-4],
       [ 1,-7, 5],
       [ 2, 2, 0]]

v1_step1 :: [[Int]]
v1_step1 = [[ 3,-1,-1],
      [ 1, 3, 3],
      [-3, 1,-3],
      [-1,-3, 1]]

ex1_step2 :: [[Int]]
ex1_step2 = [[ 5,-3,-1],
       [ 1,-2, 2],
       [ 1,-4,-1],
       [ 1,-4, 2]]

v1_step2 :: [[Int]]
v1_step2 = [[ 3,-2,-2],
      [-2, 5, 6],
      [ 0, 3,-6],
      [-1,-6, 2]]

-- <x=-8, y=-10, z=0>
-- <x=5, y=5, z=10>
-- <x=2, y=-7, z=3>
-- <x=9, y=-8, z=-3>
ex2 :: [[Int]]
ex2 = [[-8, -10,  0],
       [ 5,   5, 10],
       [ 2,  -7,  3],
       [ 9,  -8, -3]]

spec :: Spec
spec = do
    describe "updateVelocity1dim" $ do
        it "updateVelocity1dim" $ do
            updateVelocity1dim ([-1,2,4,3],2,10) `shouldBe` 11 
    describe "updateVelocity" $ do
        it "updateVelocity for example 1, first row, starting from (0,0,0)" $ do
            updateVelocity ex1 ((head ex1), [0,0,0]) `shouldBe` [3,-1,-1]
    describe "nextStep" $ do
        it "nextStep for example 1st first step" $ do
            nextStep (ex1, v1) `shouldBe` (ex1_step1, v1_step1)
        it "nextStep for example 2nd first step" $ do
            nextStep (ex1_step1, v1_step1) `shouldBe` (ex1_step2, v1_step2)
    describe "computeEnergy" $ do
        it "computes the energy for example 1 after 10 steps" $ do
            computeEnergy ex1 10 `shouldBe` 179
    describe "computeEnergy" $ do
        it "computes the energy for example 2 after 100 steps" $ do
            computeEnergy ex2 100 `shouldBe` 1940

    describe "findRepeat" $ do
        it "uses brute force to find repeating locations/velocity" $ do
            findRepeat ex1 `shouldBe` 2772

    describe "findRepeatCol" $ do
        it "uses brute force to find repeating locations/velocity for column 1" $ do
            findRepeatCol ex1 0 `shouldBe` 18
        it "uses brute force to find repeating locations/velocity for column 2" $ do
            findRepeatCol ex1 1 `shouldBe` 28
        it "uses brute force to find repeating locations/velocity for column 2" $ do
            findRepeatCol ex1 2 `shouldBe` 44
