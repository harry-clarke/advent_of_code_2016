module Puzzle_9 where

import System.IO
import Data.List
import Data.Char

type Span = Int
type Count = Int
type DataSec = String
type Rest = String
data Instr = Instr Span Count DataSec Rest

break' :: (a -> Bool) -> [a] -> ([a] , [a])
break' f l = let (a , b) = break f l in (a , tail b)

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' i l
  | length l >= i = splitAt i l
  | otherwise = error "Instruction overuns."

parseInstruction :: String -> Maybe Instr
parseInstruction ('(':rest) =
  Just (Instr span count dataSec rest')
  where
    b = break' (=='x') rest
    b' = break' (==')') $ snd b
    span = read $ fst b
    count = read $ fst b'
    (dataSec , rest') = splitAt' span $ snd b'
parseInstruction _ = Nothing

answer_1 :: String -> String
answer_1 "" = ""
answer_1 s
  | Just (Instr span count sec rest) <- parseInstruction s =
    let pre = take (length sec * count) $ cycle sec in
    (pre ++ answer_1 rest)
  | (c:cs) <- s = (c : answer_1 cs)

answer_2 :: String -> Int
answer_2 "" = 0
answer_2 s
  | Just (Instr span c sec rest) <- parseInstruction s =
    c * answer_2 sec + answer_2 rest
  | otherwise = 1 + answer_2 (tail s)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let i = filter (not . isSpace) contents
  let a1 = length $ answer_1 i
  let a2 = answer_2 i
  print a1
  print a2
