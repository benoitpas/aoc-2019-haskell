module Day10Spec (spec) where

import Test.Hspec
import Day10

import qualified Data.Set as S

ex1 :: [String]
ex1 = [ ".#..#",
        ".....",
        "#####",
        "....#",
        "...##"]

ex1points = [(1,0),(4,0),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(3,4),(4,4)]

ex1b = ([(4,0),(0,2),(1,2),(3,2),(4,2),(4,3),(3,4),(4,4)],S.fromList [(2,2)],S.fromList [(1,0)])

ex1c = ([(0,2),(1,2),(3,2),(4,2),(4,3),(3,4),(4,4)],S.fromList [(2,2),(4,0)],S.fromList [(1,0)])

ex1d = ([(1,2),(3,2),(4,2),(4,3),(3,4),(4,4)],S.fromList [(0,2),(2,2),(4,0)],S.fromList [(1,0)])

ex1e = ([(3,2),(4,2),(4,3),(3,4),(4,4)],S.fromList [(1,2),(0,2),(2,2),(4,0)],S.fromList [(1,0)])

ex2b = ([(4,0),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(3,4),(4,4)],S.fromList [(1,0)],S.fromList [])
ex2c = ([(0,2),(1,2),(2,2),(3,2),(4,2),(3,4)],S.fromList [(1,0),(4,3)],S.fromList [(4,0),(4,4)])
ex2d = ([(4,2),(3,4)],S.fromList [(1,0),(3,2),(4,3)],S.fromList [(0,2),(1,2),(2,2),(4,0),(4,4)])
ex2e =  ([],S.fromList [(1,0),(3,2),(4,2),(4,3)],S.fromList [(0,2),(1,2),(2,2),(3,4),(4,0),(4,4)])

spec :: Spec
spec = do
  describe "toPoints" $ do
    it "extract points from grid" $ do
      toPoints ex1 `shouldBe` ex1points
  
  describe "aligned" $ do
    it "check if points are aligned 1" $ do
      aligned (2,1) (4,2) (6,3) `shouldBe` True
    it "check if points are aligned 2" $ do
      aligned (4,2) (2,1) (6,3) `shouldBe` True
    it "check if points are aligned 3" $ do
      aligned (6,3) (4,2) (2,1) `shouldBe` True
    it "check if points are not aligned" $ do
      aligned (2,1) (4,2) (6,2) `shouldBe` False

  describe "findAligned" $ do
    it "find aligned points ex1 step 1" $ do
      findAligned (3,4) (ex1points, S.empty, S.empty) `shouldBe` ex1b 
    it "find aligned points ex1 step 2" $ do
      findAligned (3,4) ex1b `shouldBe` ex1c
    it "find aligned points ex1 step 3" $ do
      findAligned (3,4) ex1c `shouldBe` ex1d
    it "find aligned points ex1 step 4" $ do
      findAligned (3,4) ex1d `shouldBe` ex1e

    it "find aligned points ex2 step 1" $ do
      findAligned (4,2) (ex1points, S.empty, S.empty) `shouldBe` ex2b 
    it "find aligned points ex2 step 2" $ do
      findAligned (4,2) ex2b `shouldBe` ex2c
    it "find aligned points ex2 step 3" $ do
      findAligned (4,2) ex2c `shouldBe` ex2d
    it "find aligned points ex2 step 4" $ do
      findAligned (4,2) ex2d `shouldBe` ex2e
