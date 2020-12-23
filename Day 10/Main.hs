module Main where

import Data.List
import System.Environment (getArgs)

differences [i] = []
differences (i : is@(j : _)) = (j - i) : differences is

sortedFile :: String -> IO [Int]
sortedFile f = do s <- readFile f
                  return (sort (map read (lines s)))

-- Brute force works for small cases

choices :: [Int] -> [[Int]]
choices [] = []
choices [i] = [[i]]
choices (i : is) = do js <- jss
                      ks <- choices js
                      return (i : ks)
    where jss = filter ((<= i + 3) . head) $ take 3 $ init $ tails is

-- Full disclosure: I had to get the hint to look at subsections of the full graph on Reddit :(

groups :: [Int] -> [[Int]]
groups [i] = [[i]]
groups (i : is)
    | i == j - 1 = (i : js) : ks
    | otherwise  = [i] : js : ks
    where (js@(j : _) : ks) = groups is

main = do is <- sortedFile . head =<< getArgs
          let ds = differences (0 : is)
              ones = length (filter (1 ==) ds)
              threes = length (filter (3 ==) ds) + 1
          putStrLn (unlines ["Part 1: " ++ show ones ++ " * " ++ show threes ++ " = " ++ show (ones * threes),
                             "Part 2: " ++ show (product (map (length . choices) (groups (0 : is))))])
