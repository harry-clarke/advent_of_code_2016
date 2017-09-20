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

parseInstruction :: String -> Maybe Instr
parseInstruction ('(':rest) =
  Just (Instr span count dataSec rest')
  where
    b = break' (=='x') rest
    b' = break' (==')') $ snd b
    span = read $ fst b
    count = read $ fst b'
    (dataSec , rest') = splitAt span $ snd b'
parseInstruction _ = Nothing

convert :: String -> String
convert "" = ""
convert s
  | Just (Instr span count sec rest) <- parseInstruction s =
    let pre = take (length sec * count) $ cycle sec in
    (pre ++ convert rest)
  | (c:cs) <- s = (c : convert cs)

{-
count :: String -> Int
count "" = 0
count s
  | Just ((span,count), rest) <- parseInstruction s =
    let (sec , rest') = splitAt span rest in
-}

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let i = filter (not . isSpace) contents
  let a = length $ convert i
  print a
