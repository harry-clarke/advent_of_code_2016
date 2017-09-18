module Puzzle_4 where

import System.IO
import Data.Char
import Data.List

type Checksum = String
type SectorId = Int
type Message = (String , SectorId , Checksum)

dropLast :: Int -> [a] -> [a]
dropLast 0 l = l
dropLast n l = take s l
  where s = length l - n

sortPopularity :: (Ord a, Eq a) => [a] -> [a]
sortPopularity = map (!! 0) . sortOn (negate . length) . group . sort

isValid :: Message -> Bool
isValid (m , _ , c) =
  let m' = (take 5 . sortPopularity $ filter (/= '-') m) in
  let b = c == m' in b

parseMessage :: String -> Message
parseMessage s =
  let (m , s') = break isNumber s in
  let m' = dropLast 1 m in
  let (sid , chk) = span isNumber s' in
  let ('[':chk') = dropLast 1 chk in
  (m' , read sid , chk')

decryptChar :: Int -> Char -> Char
decryptChar 0 c = c
decryptChar _ '-' = ' '
decryptChar i 'z' = decryptChar (i-1) 'a'
decryptChar i c | i >= 26 = decryptChar (i `mod` 26) c
decryptChar i c | d <- ord 'z' - ord c, d<i = decryptChar (i-d-1) 'a'
decryptChar i c = chr(ord c + i)

decrypt :: Message -> Message
decrypt (m , sid , chk) = (map (decryptChar sid) m , sid , chk)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let l = lines contents
  let m = map parseMessage l
  let v = filter isValid m
  let s = sum $ map (\(_,x,_) -> x) v
  let d = filter (isInfixOf "north" . \(m,_,_) -> m) $ map decrypt v
  print s
  print d

