module Main where

import System.Environment

tree right down (s, n) = n `mod` down == 0 && s !! (right * (n `div` down) `mod` length s) == '#'

trees right down ss = length (filter (tree right down) (zip ss [0..]))

main = do s <- readFile . head =<< getArgs
          let ss = lines s
          putStrLn (unlines ["Part 1: " ++ show (trees 3 1 ss),
                             "Part 2: " ++ show (product [trees right down ss | (right, down) <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]])])
