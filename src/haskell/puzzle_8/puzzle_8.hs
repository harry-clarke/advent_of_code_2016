module Puzzle_8 where

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as M

type Screen = M.Map (Int,Int) Bool

screen_width = 50
screen_height = 6

emptyScreen = M.fromList [((x,y),False) | x <- [0..screen_width-1], y <- [0..screen_height-1]]

data Instruction
  = Rect Int Int
  | RotateRow Int Int
  | RotateCol Int Int
  deriving (Show, Eq)

parseInstruction :: String -> Instruction
parseInstruction s
  | Just s <- stripPrefix "rect " s =
    let (w,('x':h)) = break (=='x') s in
    Rect (read w) (read h)
  | Just s' <- stripPrefix "rotate row y=" s,
    (y,n) <- parseRotate s' = RotateRow y n
  | Just s' <- stripPrefix "rotate column x=" s,
    (x,n) <- parseRotate s' = RotateCol x n
  where
    parseRotate :: String -> (Int , Int)
    parseRotate s =
      let (v,rest) = break (==' ') s in
      let Just n = stripPrefix " by " rest in (read v , read n)

move :: Instruction -> Screen -> Screen
move (Rect w h) s =
  let rect = [(x,y) | x <- [0..w-1], y <- [0..h-1]] in
  foldr (\(x,y) s -> M.insert (x,y) True s) s rect

move (RotateRow y n) s =
  foldr rotate s [0..screen_width-1]
  where
  rotate :: Int -> Screen -> Screen
  rotate x s' =
    let v = fromJust $ M.lookup (x, y) s in
    let kx = (x+n) `mod` screen_width in
    M.insert (kx,y) v s'

move (RotateCol x n) s =
  foldr rotate s [0..screen_height-1]
  where
  rotate :: Int -> Screen -> Screen
  rotate y s' =
    let v = fromJust $ M.lookup (x, y) s in
    let ky = (y+n) `mod` screen_height in
    M.insert (x,ky) v s'

printScreen :: Screen -> IO ()
printScreen s =
  void . sequence $ [printRow y s | y <- [0..screen_height-1]]

printRow :: Int -> Screen -> IO ()
printRow y s = do
  void $ sequence [putChar (if b then '#' else '-') | x <- [0..screen_width-1], let Just b = M.lookup (x,y) s]
  putChar '\n'

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let l = lines contents
  let p = map parseInstruction l
  {-
  let p =
        [ Rect 5 5
        , RotateRow 4 10 ]
  -}
  let a = foldr move emptyScreen $ reverse p
  let a1 = length . filter id $ M.elems a
  print a1
  printScreen a
