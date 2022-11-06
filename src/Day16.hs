module Day16
    (
        pattern, onePhase, output,
        run
    ) where

pattern :: Num a => Int -> [a]
pattern index = 
    let p = [0, 1, 0, -1] >>= (\v -> replicate index v)
    in tail (cycle p)


onePhase :: Integral a => [a] -> [a]
onePhase digits = 
    map (\i -> abs (sum (map (\(a,b) -> a * b) (digits `zip` pattern i))) `mod` 10) [1..length digits]

readInteger:: Char -> Int
readInteger c = read [c]

output:: String -> Int -> String
output input nbPhases =
    let digits = map readInteger input in
    let r = last $ take (nbPhases + 1) (iterate onePhase digits)
    in take 8 $ map (head . show) r
        
--output2:: String -> Int -> String
--output2 input nbPhases =
--    let digits = map readInteger input in
--    let digits10000 = take (10000 * length input) (concat (repeat input)) in digits10000
--    let r = last $ take (nbPhases + 1) (iterate onePhase digits10000)
--    in take 8 $ map (head . show) r

run :: IO ()
run = do
    content <- readFile "src/day16_input.txt"
    let input = head (lines content)
    let p1 = output input 100
    print ("puzzle 1=" ++ p1)