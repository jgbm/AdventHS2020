module Main where

import Data.List
import System.Environment (getArgs)

invalid :: [Int] -> [Int] -> Int
invalid is [] = error "No invalid values"
invalid is (j : js) | j `elem` sums = invalid (j : init is) js
                    | otherwise     = j
    where sums = [k + l | (k : ls) <- tails is, l <- ls]

sumsTo :: Int -> [Int] -> [Int]
sumsTo n is = search iss
    where iss = map (drop 2 . inits) (tails is)
          search []         = error "None"
          search ([] : kss) = search kss
          search ((js : jss) : kss)
              | m < n       = search (jss : kss)
              | m == n      = js
              | m > n       = search kss
              where m = sum js

main = do pres : fileName : _ <- getArgs
          s <- readFile fileName
          let pre = read pres :: Int
              is  = map read (lines s) :: [Int]
              i   = invalid (reverse (take pre is)) (drop pre is)
              js  = sumsTo i is
          putStrLn (unlines ["Part 1: " ++ show i, "Part 2: " ++ show (minimum js + maximum js)])
