module Lesson_2 where

import System.IO
import Data.List

data Number = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Read, Eq)
data Move = L | R | U | D deriving (Show, Read, Eq)

move L One = One
move L Two = One
move L Three = Two

move L Four = Four
move L Five = Four
move L Six = Five

move L Seven = Seven
move L Eight = Seven
move L Nine = Eight

move R One = Two
move R Two = Three
move R Three = Three

move R Four = Five
move R Five = Six
move R Six = Six

move R Seven = Eight
move R Eight = Nine
move R Nine = Nine

move U One = One
move U Two = Two
move U Three = Three

move U Four = One
move U Five = Two
move U Six = Three

move U Seven = Four
move U Eight = Five
move U Nine = Six

move D One = Four
move D Two = Five
move D Three = Six

move D Four = Seven
move D Five = Eight
move D Six = Nine

move D Seven = Seven
move D Eight = Eight
move D Nine = Nine

findNumber :: Number -> [ Move ] -> Number
findNumber = foldr move

parseLine :: String -> [ Move ]
parseLine = map (\x -> read [x])

findNumbers :: [ [ Move ] ] -> Number -> [ Number ]
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
  --let ms = [reverse [D,D,D,U,U,U]]
  let a1 = findNumbers ms Five
  print a1
