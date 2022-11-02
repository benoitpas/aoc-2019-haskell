module Day13
    (
        nextOutput,
        Tile(..), toTiles, paddleDirection,
        run, run2
    ) where

import Control.Concurrent
import qualified Data.Map as M
import System.Console.ANSI

import Day3 (Point)
import Day5 (stateFromProgram, allSteps, nextStep, State(..))

data Tile = Empty Point | Wall Point| Block Point| Paddle Point | Ball Point | Score Int deriving (Eq, Show)  

toTiles :: [Integer] -> [Tile]
toTiles l = case l of
            -1: 0: score:remain-> Score (fromIntegral score) : toTiles remain
            x : y: 1:remain -> Wall (fromIntegral x, fromIntegral y) : toTiles remain
            x : y: 2:remain -> Block (fromIntegral x, fromIntegral y) : toTiles remain
            x : y: 3:remain -> Paddle (fromIntegral x, fromIntegral y) : toTiles remain
            x : y: 4:remain -> Ball (fromIntegral x, fromIntegral y) : toTiles remain
            x : y: _:remain -> Empty (fromIntegral x, fromIntegral y) : toTiles remain
            _ -> []

isBlock :: Tile -> Bool
isBlock (Block _) = True
isBlock _ = False

nextOutput :: Int -> State -> (State, Integer)
nextOutput nOutput state = last $ takeWhile (\(s,c) -> length (output s) <= nOutput && c /= 99) (iterate (\(s, _) -> nextStep s) (state {output = []}, 0))
--nextOutput nOutput state = until (\(s,c) -> length (output s) == nOutput || c == 99) (\(s, _) -> nextStep s) (state {output = []}, 0)

nextTile :: State -> (State, Integer)
nextTile = nextOutput 3

paddleDirection :: [Point] -> Point -> [Point] -> Integer
paddleDirection balls ballDirection paddle =
    let direction xb xp = if xb > xp then 1
                          else if xb < xp then -1
                          else 0
    in case (balls, paddle) of
        (b:_, p:_) -> direction (fst ballDirection + fst b) (fst p)

        _ -> toInteger (fst ballDirection)

printTile :: Tile -> IO ()
printTile t = case t of
                Empty p  -> oneTile p " " 0--10000
                Wall p   -> oneTile p "#" 0
                Block p  -> oneTile p "+" 0
                Paddle p -> oneTile p "=" 0--1000000
                Ball p   -> oneTile p "*" 0--1000000
                Score s  -> do
                    setCursorPosition 0 xLabels
                    putStr "Score"
                    setCursorPosition 1 xLabels
                    putStr (show s ++ "    ")
              where
                oneTile p c d = do
                    setCursorPosition (snd p) (fst p)
                    putStr c
                    threadDelay d

xLabels :: Int
xLabels = 40

printPoint :: String -> Int -> [Point] -> IO ()
printPoint label yLabel points = do
    setCursorPosition yLabel xLabels
    putStr label
    setCursorPosition (yLabel + 1) xLabels
    let s = case points of
                p:_ -> show (fst p) ++ "," ++ show (snd p) ++ "  "
                _ -> ""
    putStr s

processNextTile :: (State, [Point], [Point], Int, Bool, IO ()) -> (State, [Point], [Point], Int, Bool, IO ())
processNextTile (state, balls, paddle, score, finished, io) = 
    let ballDirection = let m = memory state in (fromInteger (m M.! 390), fromInteger (m M.! 391)) in
    let (nState, nCmd) = nextTile state {input = paddleDirection balls ballDirection paddle} in
    let nFinished = (nCmd == 99) || finished in
    let nTiles = (toTiles . output) nState in
    let nIO = io >>= (\_ -> do
            printPoint "Ball" 3 balls
            printPoint "Ball direction" 6 [ballDirection]
            printPoint "Paddle" 9 paddle
            case nTiles of
                [tile] -> printTile tile
                _ -> return ())

    in case nTiles of
        [] -> (nState, balls, paddle, score, True, nIO)
        [Ball newBall] -> let nBalls = newBall:balls in (nState, nBalls, paddle, score, nFinished, nIO)
        [Paddle newPaddle] -> (nState, balls, newPaddle:paddle, score, nFinished, nIO)
        [Score nScore] -> (nState, balls, paddle, nScore, nFinished, nIO)
        _ -> (nState, balls, paddle, score, nFinished, nIO)

run2 :: IO ()
run2 = readFile "src/day13_input.txt" >>= 
    (\content -> let iState = stateFromProgram (head (lines content)) 0 
    in let iState2 = iState { memory = M.insert 0 2 (memory iState) }
    in let ts = filter (\(_,_,_,_,f,_) -> f) $ iterate processNextTile (iState2, [], [], 0, False, do { clearScreen ; hideCursor })
    in let (_,_,_,_,_,io) = head ts
    in io >>= (\_ -> showCursor)
          >>= (\_ -> setSGR [Reset])
          >>= (\_ -> setCursorPosition 23 0)
          >>= (\_ -> putStrLn "Finished !"))

run :: IO ()
run = do
    content <- readFile "src/day13_input.txt"
    let iState = stateFromProgram (head (lines content)) 0
    let state1 = allSteps iState
    let iTiles = toTiles (output state1)
    let iBlocks =  filter isBlock iTiles
    print ("puzzle 1: " ++ show (length iBlocks))