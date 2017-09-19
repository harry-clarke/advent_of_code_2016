module Puzzle_3 where

import Data.List
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let c = transpose $ lines contents
  let p = map (map head . sortOn length . group . sort) c
  let a1 = map (head . reverse) p
  let a2 = map head p
  print a1
  print a2
