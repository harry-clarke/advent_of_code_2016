module Puzzle_1 where

import Data.Maybe
import Data.List

input = "R4, R3, L3, L2, L1, R1, L1, R2, R3, L5, L5, R4, L4, R2, R4, L3, R3, L3, R3, R4, R2, L1, R2, L3, L2, L1, R3, R5, L1, L4, R2, L4, R3, R1, R2, L5, R2, L189, R5, L5, R52, R3, L1, R4, R5, R1, R4, L1, L3, R2, L2, L3, R4, R3, L2, L5, R4, R5, L2, R2, L1, L3, R3, L4, R4, R5, L1, L1, R3, L5, L2, R76, R2, R2, L1, L3, R189, L3, L4, L1, L3, R5, R4, L1, R1, L1, L1, R2, L4, R2, L5, L5, L5, R2, L4, L5, R4, R4, R5, L5, R3, L1, L3, L1, L1, L3, L4, R5, L3, R5, R3, R3, L5, L5, R3, R4, L3, R3, R1, R3, R2, R2, L1, R1, L3, L3, L3, L1, R2, L1, R4, R4, L1, L1, R3, R3, R4, R1, L5, L2, R2, R3, R2, L3, R4, L5, R1, R4, R5, R4, L4, R1, L3, R1, R3, L2, L3, R1, L2, R3, L3, L1, L3, R4, L4, L5, R3, R5, R4, R1, L2, R3, R5, L5, L4, L1, L1"

parse :: String -> [ Instruction ]
parse str =
  let (x , xs) = break (== ',') str in
  case xs of
    [] -> [parse' x] 
    (',':' ':xs) -> (parse' x : parse xs)
  where parse' (m:i) = (read [m] , read i)

data Direction = N | E | S | W deriving (Show, Eq)
type Location = (Direction, Int, Int) {- Facing, X coord, Y coord -}
data Move = R | L deriving (Show, Read, Eq)
type Instruction = (Move, Int)

face :: Direction -> Move -> Direction
face N R = E
face E R = S
face S R = W
face W R = N
face N L = W
face W L = S
face S L = E
face E L = N

{-
 - Move forward the given amount, listing each location we visit along the way.
 -}
forward :: Location -> Int -> [ Location ]
forward (d , x , y) i =
  let (dx , dy) = case d of
                    N -> (0 , 1)
                    E -> (1 , 0)
                    S -> (0 , -1)
                    W -> (-1 , 0)
  in
  reverse [(d , rx , ry) | n<-[1..i], let rx = x + n*dx, let ry = y + n*dy]

{-
 - Follows an instruction, facing the given direction and moving forward the given number of moves.
 -}
move :: Instruction -> [ Location ] -> [ Location ]
move (_ , 0) ls = ls
move (m , n) (l@(d , x , y):ls) = forward (face d m , x , y) n ++ l:ls


run :: String -> [ Location ]
run = foldr move [(N , 0 , 0)] . reverse . parse

distance :: (Int , Int) -> Int
distance (x , y) = abs x + abs y

coords :: Location -> (Int , Int)
coords (_ , x , y) = (x , y)

answer1b :: [ Location ] -> Int
answer1b ls =
  let lls = reverse $ map coords ls in
  let g = map (!! 1) $ filter ((/= 1) . length) (group $ sort lls) in
  distance . fromJust $ find (flip elem $ g) lls


main :: IO ()
main =
  let ls@(a1:_) = run input in
  let a2 = answer1b ls in
  do
    putStrLn ("Answer 1a: " ++ show (distance $ coords a1))
    putStrLn ("Answer 1b: " ++ show a2)
