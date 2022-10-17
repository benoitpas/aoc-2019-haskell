module Day13
    (
        toTiles,
        run
    ) where

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

import Day3 (Point)
import Day5 (stateFromProgram, allSteps, State(..))
import Day7 (nextSteps)

data Tile = Empty | Wall | Block | Paddle | Ball deriving (Eq, Show)  

toTile :: Integer -> Tile
toTile i = case i of
    1 -> Wall
    2 -> Block
    3 -> Paddle
    4 -> Ball
    _ -> Empty

toTiles :: [Integer] -> [((Integer, Integer), Tile)]
toTiles [] = []
toTiles codes = ((x,y), (toTile tile)):(toTiles remain)
                    where (x:y:tile:_,remain) = L.splitAt 3 codes

run :: IO ()
run = do
    content <- readFile "src/day13_input.txt"
    let iState = stateFromProgram (head (lines content)) 0
    let state1 = allSteps iState
    let iTiles = toTiles (output state1)
    let filterTile tile = filter (\(_,t) -> t == tile)
    let iBlocks =  filterTile Block iTiles
    print ("puzzle 1: " ++ show (length iBlocks))
    let ball =  fst (head (filterTile Ball iTiles))
    print ( "Ball " ++ show ball)
    let paddle =  fst (head (filterTile Paddle iTiles))
    print ( "Paddle " ++ show paddle)
    -- part 2
    let iState2 = iState { memory = M.insert 0 2 (memory iState)}
    let state2 = allSteps iState2
    let iTiles2 = toTiles (output state2)
    let balls =  (filterTile Ball iTiles2)
    print ( "Ball " ++ show balls)
    let paddles =  (filterTile Paddle iTiles2)
    print ( "Paddle " ++ show paddles)
 