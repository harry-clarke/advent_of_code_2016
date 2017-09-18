module NumPad where

class NumPad a where
  move :: Move -> a -> a
  startingPoint :: a

data Move = L | R | U | D deriving (Show, Read, Eq)

parseLine :: String -> [ Move ]
parseLine = map (\x -> read [x])
