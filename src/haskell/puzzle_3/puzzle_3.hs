module Puzzle_3 where

import System.IO

type Triple = (Int , Int , Int)

isTriangle :: Triple -> Bool
isTriangle (a , b , c)
  = (a + b) > c && (b + c) > a && (a + c) > b

parseLine :: String -> Triple
parseLine l = let (a:b:c:[]) = map read $ words l in (a , b , c)

streamToTriple :: [ Int ] -> [ Triple ]
streamToTriple [] = []
streamToTriple (a:b:c:rest) = (a , b , c) : streamToTriple rest

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let l = lines contents
  let p1 = map parseLine l
  let a1 = filter isTriangle p1
  let p2 = streamToTriple . (\(a,b,c) -> a ++ b ++ c) $ unzip3 p1
  let a2 = filter isTriangle p2
  print $ length a1
  print $ length a2
