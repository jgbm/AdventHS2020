module Main where

import Control.Monad.State
import Data.Char
import System.Environment (getArgs)

parse :: String -> (Int, Int, Char, String)
parse = evalState (do lows <- split isDigit
                      count 1
                      highs <- split isDigit
                      cs <- count 2
                      count 2
                      s <- get
                      return (read lows, read highs, cs !! 1, s))
    where split :: (Char -> Bool) -> State String String
          split p = do xs <- get
                       put (dropWhile p xs)
                       return (takeWhile p xs)
          count :: Int -> State String String
          count n = do xs <- get
                       put (drop n xs)
                       return (take n xs)

countPassing p = length . filter id . map p

part1 (low, high, c, cs) = n >= low && n <= high
    where n = length (filter (c ==) cs)

part2 (low, high, c, cs) = (cs !! (low - 1) == c) /= (cs !! (high - 1) == c)

main = do s <- readFile . head =<< getArgs
          let quads = map parse (lines s)
          putStrLn (unlines ["Part 1: " ++ show (countPassing part1 quads),
                             "Part 2: " ++ show (countPassing part2 quads)])
