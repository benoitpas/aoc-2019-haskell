module Day13
    (
        Tile(..), toTiles, paddleDirection,
        run
    ) where

import System.Console.ANSI
import qualified Data.Map as M

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
            [] -> []

isBlock :: Tile -> Bool
isBlock (Block _) = True
isBlock _ = False

isBall :: Tile -> Bool
isBall (Ball _) = True
isBall _ = False

isPaddle :: Tile -> Bool
isPaddle (Paddle _) = True
isPaddle _ = False

nextTile :: State -> (State, Integer)
nextTile state = last $ takeWhile (\(s,c) -> length (output s) <= 3 && c /= 99) (iterate (\(s, _) -> nextStep s) (state {output = []}, 0))

paddleDirection :: [Point] -> [Point] -> Integer
paddleDirection balls paddle = 
    let direction xb xp = if xb > xp then 1
                          else if xb < xp then -1
                          else 0
    in case (balls, paddle) of
        (b1:b2:_, p:_) -> if (snd b1) >= (snd b2) then
                            direction (fst b1) (fst p)
                          else
                            let xTarget = (fst b1) -- + (snd p) - (snd b1) 
                            in direction xTarget (fst p)

--        (b:_, p:_) -> toInteger $ (fst b - fst p) `div` (abs (fst b - fst p))
        _ -> 1

processNextTile :: (State, [Point], [Point], Int, Bool, IO ()) -> (State, [Point], [Point], Int, Bool, IO ())
processNextTile (state, balls, paddle, score, finished, io) = 
    let (nState, nCmd) = nextTile state in
    let nIo = io >>= (\_ -> putStr ".") in
    let nFinished = (nCmd == 99) || finished
    in case (toTiles . output) nState of
        [] -> (nState, balls, paddle, score, True, nIo)
        [Ball newBall] -> let nBalls = newBall:balls in (nState {input = paddleDirection balls paddle}, nBalls, paddle, score, nFinished, nIo)
        [Paddle newPaddle] -> (nState {input = paddleDirection balls [newPaddle]}, balls, newPaddle:paddle, score, nFinished, nIo)
        [Score nScore] -> (nState, balls, paddle, nScore, nFinished, nIo)
        _ -> (nState, balls, paddle, score, nFinished, nIo)

--nextBall state = let nState = nextTile state in if (isBall . last . toTiles . output) nState then nState else nextBall nState

--findTile isTile state = head (filter isTile (tail (iterate nextTile state )))

run :: IO ()
run = do
    content <- readFile "src/day13_input.txt"
    let iState = stateFromProgram (head (lines content)) 0
    let state1 = allSteps iState
    let iTiles = toTiles (output state1)
    let iBlocks =  filter isBlock iTiles
    print ("puzzle 1: " ++ show (length iBlocks))
    let ball =  filter isBall iTiles
    print ( "Ball " ++ show ball)
    let paddle =  filter isPaddle iTiles
    print ( "Paddle " ++ show paddle)

    -- part 2
    clearScreen
    setSGR [SetColor Foreground Vivid Blue]
    let iState2 = iState { memory = M.insert 0 2 (memory iState)}

    let ts = filter (\(_,_,_,s,f,_) -> s>0) $ iterate processNextTile (iState2, [], [], 0, False, clearScreen)
    let (lastState,_,_,_,_,_) = head ts
    print (show lastState)
--    let ts2 = filter (\(_,_,_,s,f,_) -> s==0) $ iterate processNextTile lastState
--    print (show (head ts2))

--    let (ls1,_,_,_,_) = lastState
--    print (show (output s814))
--    let (s815,f815) = nextStep s814
--    print (show (output s815)++ " "++show f815)
--    let (s816,f816) = nextStep s815
--    print (show (output s816)++ " "++show f816)
--    let (s817,f817) = nextStep s816
--    print (show ( output s817)++ " "++show f817)
--    let (s818,f818) = nextStep s817
--    print (show (output s818)++ " "++show f818)
--    let end = allSteps s817
--    print (show end)
    --let score = case L.find (\(_,_,_,_, f) -> f) ts of
--                    Just (_,_,_,score,_) -> score
--                    _ -> 0
    --print (show score)

    --let stateBall = nextBall iState2
    --print ("stateBall=" ++ show (output stateBall))
    --let statePaddle = findTile (isPaddle . head . toTiles . output) stateBall
    --print ("statePaddle=" ++ show (output statePaddle))
    --let state3 = allSteps statePaddle { input = 1 }
    --let tiles3 = toTiles (output state3)
    --let balls =  filter isBall tiles3
    --print (show balls)
    --let paddles =  filter isPaddle tiles3
    
    --print ("Paddle " ++ show paddles)
    --print (show tiles3)