module Main where

import System.Environment (getArgs)

groups :: [String] -> [[String]]
groups [] = []
groups ss = first : groups rest
    where first = takeWhile ("" /=) ss
          rest = dropWhile ("" ==) (drop (length first) ss)

answers :: String -> Char -> Bool
answers [] _     = False
answers (c:cs) b = b == c || answers cs b

countAny :: String -> Int
countAny s = length (filter (answers s) ['a'..'z'])

countAll :: [String] -> Int
countAll ss = length (filter (\c -> all ($ c) (map answers ss)) ['a'..'z'])

main = do s <- readFile . head =<< getArgs
          let gs = (groups . lines) s
          putStrLn (unlines ["Part 1: " ++ (show . sum . map (countAny . concat)) gs,
                             "Part 2: " ++ (show . sum . map countAll) gs])
