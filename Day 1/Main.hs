module Main where

import Data.List (tails)
import System.Environment (getArgs)

part1 xs = [x * y | x:ys <- tails xs, y <- ys, x + y == 2020]

part2 xs = [x * y * z | x:ys <- tails xs, y:zs <- tails ys, z <- zs, x + y + z == 2020]

main = do s <- readFile . head =<< getArgs
          let xs = map read (lines s)
              p1 = part1 xs
              p2 = part2 xs
          putStrLn (unlines ["Part 1: " ++ show p1, "Part 2: " ++ show p2])
