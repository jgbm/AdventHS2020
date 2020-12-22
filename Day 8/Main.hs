module Main where

import Data.Array
import Data.Either
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

data Instr = Nop Int | Acc Int | Jmp Int
  deriving Show

instr :: ReadP Instr
instr = do cf   <- (string "nop " >> return Nop) +++
                   (string "acc " >> return Acc) +++
                   (string "jmp " >> return Jmp)
           sign <- (string "+" >> return id) +++
                   (string "-" >> return negate)
           arg  <- int
           eof
           return (cf (sign arg))
    where int :: ReadP Int
          int = readS_to_P reads

instrs :: String -> [Instr]
instrs s = case sequence (map parseInstr (lines s)) of
             Nothing -> error "Parse error"
             Just is -> is
    where parseInstr s = case readP_to_S instr s of
                           ((i, "") : _) -> Just i
                           _             -> Nothing

instArray :: [Instr] -> Array Int Instr
instArray is = array (0, length is - 1) (zip [0..] is)

data State = St{ acc :: Int, ip :: Int }

step :: Instr -> State -> State
step (Acc i) s = s{ acc = i + acc s, ip = 1 + ip s }
step (Jmp i) s = s{ ip = i + ip s }
step (Nop _) s = s{ ip = 1 + ip s }

runToEnd :: Array Int Instr -> State -> Either State State
runToEnd is s = go s []
    where go s seen
              | ip s `elem` seen = Left s
              | ip s > snd (bounds is) = Right s
              | otherwise = go (step (is ! ip s) s) (ip s : seen)

progs :: [Instr] -> [[Instr]]
progs [] = [[]]
progs (Nop i : is) = (Jmp i : is) : [Nop i : is' | is' <- progs is]
progs (Jmp i : is) = (Nop i : is) : [Jmp i : is' | is' <- progs is]
progs (Acc i : is) = [Acc i : is' | is' <- progs is]

main = do s <- readFile . head =<< getArgs
          let is = instrs s
              alts = progs is
              ia = instArray is
              altas = map instArray alts
          putStrLn (unlines ["Part 1: " ++ show (either acc acc (runToEnd ia (St 0 0))),
                             "Part 2: " ++ show (acc (head (rights (map (flip runToEnd (St 0 0)) altas))))])
