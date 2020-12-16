module Main where

import Data.List (sort)
import System.Environment (getArgs)


type Range = (Int, Int)

top, bottom :: Range -> Range
top (low, high) = (low + (high - low) `div` 2 + 1, high)
bottom (low, high) = (low, high - (high - low) `div` 2 - 1)

first f (a, b)  = (f a, b)
second f (a, b) = (a, f b)

find :: String -> (Range, Range) -> (Range, Range)
find [] = id
find ('F' : cs) = find cs . first bottom
find ('B' : cs) = find cs . first top
find ('R' : cs) = find cs . second top
find ('L' : cs) = find cs . second bottom

seat :: (Range, Range) -> (Int, Int)
seat ((r, _), (s, _)) = (r, s)

seatId :: (Range, Range) -> Int
seatId rs = r * 8 + s
    where (r, s) = seat rs

skip :: [Int] -> Int
skip (x : x' : xs)
    | x + 1 == x' = skip (x' : xs)
    | otherwise   = x + 1

main = do s <- readFile . head =<< getArgs
          let ss = lines s
              seats = map (seatId . flip find ((0, 127), (0, 7))) ss
          putStrLn (unlines ["Part 1: " ++ show (maximum seats),
                             "Part 2: " ++ show (skip (sort seats))])
