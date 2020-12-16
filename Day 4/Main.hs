module Main where

import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Read (readMaybe)

passports :: [String] -> [String]
passports [] = []
passports ss = intercalate " " first : passports rest
    where first = takeWhile ("" /=) ss
          rest = dropWhile ("" ==) (drop (length first) ss)

fieldNames :: String -> [String]
fieldNames = map (takeWhile (':' /=)) . words

fields :: String -> [(String, String)]
fields = map fieldOf . words
    where fieldOf s = (name, value)
              where name = takeWhile (':' /=) s
                    value = tail (drop (length name) s)

valid1 fs = all (`elem` fs) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

valid2 fs = and [maybe False p (lookup fld fs)  | (fld, p) <- validators]
    where validators = [("byr", maybe False (inRange 1920 2002) . readMaybe),
                        ("iyr", maybe False (inRange 2010 2020) . readMaybe),
                        ("eyr", maybe False (inRange 2020 2030) . readMaybe),
                        ("hgt", checkHeight),
                        ("hcl", checkHair),
                        ("ecl", flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
                        ("pid", checkPid)
                       ]
          inRange low high n = low <= n && n <= high
          checkHeight s =
              case reads s of
                ((inches, "in") : _) -> inRange 59 76 inches
                ((cms, "cm") : _)    -> inRange 150 193 cms
                _                    -> False
          checkHair ('#' : cs) = length cs == 6 && all (flip elem (['0'..'9'] ++ ['a'..'f'])) cs
          checkHair _          = False
          checkPid cs = length cs == 9 && all (flip elem ['0'..'9']) cs

main = do s <- readFile . head =<< getArgs
          let ps = passports (lines s)
          putStrLn (unlines ["Part 1: " ++ show ((length . filter valid1) (map fieldNames ps)),
                             "Part 2: " ++ show ((length . filter valid2) (map fields ps))])
