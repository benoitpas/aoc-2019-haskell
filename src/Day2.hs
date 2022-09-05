module Day2
    ( run, wordsWhen
    ) where

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

run :: IO ()
run = do
    content <- readFile "src/day2_input.txt"
    let program = (wordsWhen (==',') (head (lines content)))
    putStrLn (head program)
