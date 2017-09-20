module Puzzle_9 where

import System.IO
import Data.List
import Data.Char

type Span = Int
type Count = Int
type Instr = (Span , Count)

break' :: (a -> Bool) -> [a] -> ([a] , [a])
break' f l = let (a , b) = break f l in (a , tail b)

parseInstruction :: String -> Maybe (Instr , String)
parseInstruction ('(':rest) =
  let (span , rest') = break' (=='x') rest in
  let (count , rest'') = break' (==')') rest' in
  Just ((read span , read count) , rest'')
parseInstruction _ = Nothing

convert :: String -> String
convert = convert'
  where
    convert' :: String -> String
    convert' "" = ""
    convert' s
      | Just ((span,count), rest) <- parseInstruction s =
        let (sec , rest') = splitAt span rest in
        let pre = take (length sec * count) $ cycle sec in
        (pre ++ convert' rest')
      | (c:cs) <- s = (c : convert' cs)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let i = filter (not . isSpace) contents
  let a = length $ convert i
  print a
