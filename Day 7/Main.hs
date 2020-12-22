module Main where

import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP
import System.Environment (getArgs)

data Rule = R String [(Int, String)]
  deriving Show

parseRule :: String -> Maybe Rule
parseRule s = case filter (null . snd) (readP_to_S parse s) of
                ((r, _) : _) -> Just r
                _             -> Nothing

    where parse = do c <- start
                     cs <- many end +++
                           (string "no other bags." >> return [])
                     return (R c cs)

          start = getTo " bags contain "
          punct = char '.' +++ char ','
          end   = do n <- int
                     char ' '
                     color <- getTo (if n == 1 then " bag" else " bags")
                     punct
                     return (n, color)

          int :: ReadP Int
          int = readS_to_P reads
          getTo s = manyTill get (string s)

parseRules :: String -> Maybe [Rule]
parseRules = sequence . map parseRule . lines

type RevMap = M.Map String [String]
type FwdMap = M.Map String [(Int, String)]

addRevRule :: Rule -> RevMap -> RevMap
addRevRule (R c cs) m = foldr (\s m -> M.insertWith (++) s [c] m) m (map snd cs)

addFwdRule :: Rule -> FwdMap -> FwdMap
addFwdRule (R c cs) m = M.insert c cs m

revRules :: [Rule] -> RevMap
revRules rs = foldr addRevRule M.empty rs

fwdRules :: [Rule] -> FwdMap
fwdRules rs = foldr addFwdRule M.empty rs

contained :: RevMap -> String -> [String]
contained m start = iter [start] []
    where iter [] old = old
          iter now old = iter new (new ++ old)
              where new = filter (`notElem` old) . nub . concatMap (fromMaybe [] . flip M.lookup m) $ now

containsCount :: FwdMap -> String -> Int
containsCount rs c = case M.lookup c rs of
                       Nothing -> error ("Whoops: " ++ c)
                       Just cs -> 1 + sum [i * containsCount rs c | (i, c) <- cs]

main = do s <- readFile . head =<< getArgs
          let (fr, rr) = case parseRules s of
                           Nothing -> error "Parse error"
                           Just rs -> (fwdRules rs, revRules rs)
          putStrLn (unlines ["Part 1: " ++ show (length (contained rr "shiny gold")),
                             "Part 2: " ++ show (containsCount fr "shiny gold" - 1)])
