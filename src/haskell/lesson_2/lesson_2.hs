module Lesson_2 where

import System.IO
import Data.List
import NumPad
import NumPad_A
import NumPad_B

findNumber :: NumPad a => a -> [ Move ] -> a
findNumber = foldr move

findNumbers :: NumPad a => [ [ Move ] ] -> a -> [ a ]
findNumbers [] _ = []
findNumbers (m:ms) n =
  let s = findNumber n m in
  (s:findNumbers ms n)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let l = lines contents
  let ms = map (reverse . parseLine) l
  let a1 = findNumbers ms startingPoint :: [ B ]
  print a1
