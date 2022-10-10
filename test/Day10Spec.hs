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

ex1points :: [Point]
ex1points = [(1,0),(4,0),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(3,4),(4,4)]

ex1a :: ([Point], S.Set Point, S.Set Point)
ex1a = ([(1,0),(4,0),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(4,4)],S.empty, S.empty)

ex1b :: ([Point], S.Set Point, S.Set Point)
ex1b = ([(4,0),(0,2),(1,2),(3,2),(4,2),(4,3),(4,4)],S.fromList [(2,2)],S.fromList [(1,0)])

ex1c :: ([Point], S.Set Point, S.Set Point)
ex1c = ([(0,2),(1,2),(3,2),(4,2),(4,3),(4,4)],S.fromList [(2,2),(4,0)],S.fromList [(1,0)])

ex1d :: ([Point], S.Set Point, S.Set Point)
ex1d = ([(1,2),(3,2),(4,2),(4,3),(4,4)],S.fromList [(0,2),(2,2),(4,0)],S.fromList [(1,0)])

ex1e :: ([Point], S.Set Point, S.Set Point)
ex1e = ([(3,2),(4,2),(4,3),(4,4)],S.fromList [(1,2),(0,2),(2,2),(4,0)],S.fromList [(1,0)])

ex1_vap_3_4 = [(3,2),(4,0),(4,2),(4,3),(4,4),(0,2),(1,2),(2,2),(1,0)]

ex1_vap_4_2 = [(4,0),(4,3),(3,4),(3,2),(1,0),(4,4),(2,2),(1,2),(0,2)]

ex2a :: ([Point], S.Set Point, S.Set Point)
ex2a = ([(4,0),(0,2),(1,2),(2,2),(3,2),(4,3),(3,4),(4,4)],S.fromList [(1,0)],S.empty)
ex2b :: ([Point], S.Set Point, S.Set Point)
ex2b = ([(0,2),(1,2),(2,2),(3,2),(3,4)],S.fromList [(1,0),(4,0),(4,3)],S.fromList [(4,4)])
ex2c :: ([Point], S.Set Point, S.Set Point)
ex2c = ([(3,4)],S.fromList [(3,2),(1,0),(4,0),(4,3)],S.fromList [(0,2),(1,2),(2,2),(4,4)])
ex2d :: ([Point], S.Set Point, S.Set Point)
ex2d = ([],S.fromList [(3,4),(3,4),(3,2),(1,0),(4,0),(4,3)],S.fromList [(0,2),(1,2),(2,2),(4,4)])

ex3 :: [String]
ex3 = ["......#.#.",
       "#..#.#....",
       "..#######.",
       ".#.#.###..",
       ".#..#.....",
       "..#....#.#",
       "#..#....#.",
       ".##.#..###",
       "##...#..#.",
       ".#....####"]

ex4 = [".#....#####...#..",
       "##...##.#####..##",
       "##...#...#.#####.",
       "..#.....#...###..",
       "..#.#.....#....##"]

ex4points = toPoints ex4

ex4_vap_8_3 =  [(8,1),(9,0),(9,1),(10,0),(9,2),(11,1),(12,1),(11,2),(15,1),(12,2)]

ex5 = [".#..##.###...#######",
       "##.############..##.",
       ".#.######.########.#",
       ".###.#######.####.#.",
       "#####.##.#.##.###.##",
       "..#####..#.#########",
       "####################",
       "#.####....###.#.#.##",
       "##.#################",
       "#####.##.###..####..",
       "..######..##.#######",
       "####.##.####...##..#",
       ".#####..#.######.###",
       "##...#.##########...",
       "#.##########.#######",
       ".####.#.###.###.#.##",
       "....##.##.###..#####",
       ".#.#.###########.###",
       "#.#.#.#####.####.###",
       "###.##.####.##.#..##"]

ex5points = toPoints ex5

ex5vap = listVapAsteroids (11,13) ex5points

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
      findAligned (3,4) ex1a `shouldBe` ex1b 
    it "find aligned points ex1 step 2" $ do
      findAligned (3,4) ex1b `shouldBe` ex1c
    it "find aligned points ex1 step 3" $ do
      findAligned (3,4) ex1c `shouldBe` ex1d
    it "find aligned points ex1 step 4" $ do
      findAligned (3,4) ex1d `shouldBe` ex1e

    it "find aligned points ex2 step 1" $ do
      findAligned (4,2) ex2a `shouldBe` ex2b
    it "find aligned points ex2 step 2" $ do
      findAligned (4,2) ex2b `shouldBe` ex2c
    it "find aligned points ex2 step 3" $ do
      findAligned (4,2) ex2c `shouldBe` ex2d

  describe "findVisibleCount" $ do
    it "find number of visible asteroids at 3,4" $ do
      findVisibleCount (3,4) ex1points `shouldBe` 8
    it "find number of visible asteroids at 4,2" $ do
      findVisibleCount (4,2) ex1points `shouldBe` 5

  describe "findVisibleCount2" $ do
    it "find number of visible asteroids at 3,4" $ do
      findVisibleCount2 (3,4) ex1points `shouldBe` 8
    it "find number of visible asteroids at 4,2" $ do
      findVisibleCount2 (4,2) ex1points `shouldBe` 5

  describe "findMaxVisibleCount" $ do
    it "find max number of visible asteroids in example 1" $ do
      findMaxVisibleCount ex1points `shouldBe` (8, (3,4))
    it "find max number of visible asteroids in example 3" $ do
      findMaxVisibleCount (toPoints ex3) `shouldBe` (33, (5,8))

  describe "toPolar" $ do
    it "computes coordinates for 1st quadrant" $ do
      toPolar ( 1,-2) `shouldBe` (-1.1071487177940904, sqrt 5)
    it "computes coordinates for 1st quadrant" $ do
      toPolar ( 2,-1) `shouldBe` (-0.4636476090008061, sqrt 5)
    it "computes coordinates for 2nd quadrant" $ do
      toPolar ( 2, 1) `shouldBe` ( 0.4636476090008061, sqrt 5)
    it "computes coordinates for 2nd quadrant" $ do
      toPolar ( 1, 2) `shouldBe` (1.1071487177940904, sqrt 5)
    it "computes coordinates for 3rd quadrant" $ do
      toPolar (-1, 2) `shouldBe` (pi - 1.1071487177940904, sqrt 5)
    it "computes coordinates for 3rd quadrant" $ do
      toPolar (-2, 1) `shouldBe` (pi - 0.4636476090008061, sqrt 5)
    it "computes coordinates for 4th quadrant" $ do
      toPolar (-2,-1) `shouldBe` (pi + 0.4636476090008061, sqrt 5)
    it "computes coordinates for 4th quadrant" $ do
      toPolar (-1,-2) `shouldBe` (pi + 1.1071487177940904, sqrt 5)

  describe "anglesAndDistances" $ do
    it "find angles for example 1" $ do
      anglesAndDistances (3,4) ex1points `shouldBe` [
        ((-1.5707963267948966,2.0),(3,2)),
        ((-1.3258176636680323,4.123105625617661),(4,0)),
        ((-1.1071487177940904,2.23606797749979),(4,2)),
        ((-0.7853981633974482,1.4142135623730951),(4,3)),((0.0,1.0),(4,4)),
        ((3.729595257137361,3.605551275463989),(0,2)),((3.9269908169872414,2.8284271247461903),(1,2)),
        ((4.2487413713838835,2.23606797749979),(2,2)),((4.2487413713838835,4.47213595499958),(1,0))]

  describe "listVapAsteroids" $ do
    it "list asteroids in the 'vaporized' order for example 1 at (3,4)" $ do
      listVapAsteroids (3,4) ex1points `shouldBe` ex1_vap_3_4
    it "list asteroids in the 'vaporized' order for example 1 at (4,2)" $ do
      listVapAsteroids (4,2) ex1points `shouldBe` ex1_vap_4_2
    it "list asteroids in the 'vaporized' order for example 4" $ do
      take 10 (listVapAsteroids (8,3) ex4points) `shouldBe` ex4_vap_8_3
    it "find 200th asteroid in the 'vaporized' order for example 5" $ do
      (listVapAsteroids (11,13) ex5points) !! 199 `shouldBe` (8,2)

  describe "findPrimes" $ do
    it "find primes up to 36" $ do
      findPrimes 36 `shouldBe` [2,3,5,7,11,13,17,19,23,29,31]

  describe "reduce" $ do
    it "reduces (5,10)" $ do
      reduce (5,10) `shouldBe` (1,2)
    it "reduces (-12,3)" $ do
      reduce (-12,3) `shouldBe` (-4,1)
    it "reduces (-12,4)" $ do
      reduce (-12,4) `shouldBe` (-3,1)
    it "reduces (12,36)" $ do
      reduce (12,36) `shouldBe` (1,3)
    it "reduces (-10,0)" $ do
      reduce (-10,0) `shouldBe` (-1,0)
    it "reduces (0,9)" $ do
      reduce (0,9) `shouldBe` (0,1)

 
  describe "listVapAsteroids" $ do    
    it "find the first 3 asteroids in the 'vaporized' order for example 5 at (11,13)" $ do
      (take 3 ex5vap) `shouldBe` [(11,12),(12,1),(12,2)]
    it "find the 10th asteroid in the 'vaporized' order for example 5 at (11,13)" $ do
      (ex5vap !! 9) `shouldBe` (12,8)
    it "find the 20th asteroid in the 'vaporized' order for example 5 at (11,13)" $ do
      (ex5vap !! 19) `shouldBe` (16,0)
    it "find the 50th asteroid in the 'vaporized' order for example 5 at (11,13)" $ do
      (ex5vap !! 49) `shouldBe` (16,9)
    it "find the 100th asteroid in the 'vaporized' order for example 5 at (11,13)" $ do
      (ex5vap !! 99) `shouldBe` (10,16)
    it "find the 199.200,201th asteroid in the 'vaporized' order for example 5 at (11,13)" $ do
      drop 198 (take 201 ex5vap) `shouldBe` [(9,6),(8,2),(10,9)]
    it "find the 299th asteroid in the 'vaporized' order for example 5 at (11,13)" $ do
      (ex5vap !! 298) `shouldBe` (11,1)
