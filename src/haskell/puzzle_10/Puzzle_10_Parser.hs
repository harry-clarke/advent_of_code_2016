module Puzzle_10_Parser where

import System.IO
import qualified Data.Map as M
import Data.Either (lefts, rights)
import Data.Char
import Data.List
import Text.Regex.TDFA

type Value = Int
data Destination
  = DestBot Id
  | DestOut Id
  deriving (Show, Eq)

data Bot = Bot {
  lowerId :: Destination, -- Lower Bot
  upperId :: Destination, -- Upper Bot
  val1 :: (Maybe Value), -- First Value
  val2 :: (Maybe Value) -- Second Value
  } deriving (Show, Eq)
type Id = Int
type Bots = M.Map Id Bot
type BotDescription = (Id, Bot)

type Move = (Value, Destination)
type Moves = [Move]

data Env = Env Bots Moves deriving (Show, Eq)

readFile :: String -> IO Env
readFile fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  return $ parseFile contents

parseFile :: String -> Env
parseFile str =
  let parsed = map parseLine $ lines str in
  Env (M.fromList $ lefts parsed) (rights parsed)

parseLine :: String -> Either BotDescription Move
parseLine str
  | isPrefixOf "bot" str = Left $ parseBot str
  | isPrefixOf "value" str = Right $ parseInput str
  | otherwise = error "Nope"

parseBot :: String -> BotDescription
parseBot str =
  let pat = "bot ([0-9]+) gives low to ([a-z]+ [0-9]+) and high to ([a-z]+ [0-9]+)" in
  case str `fullMatch` pat of
    (_ : sourceId : lowBot : highBot : []) ->
      let bot = Bot (parseDest lowBot) (parseDest highBot) Nothing Nothing in
      (read sourceId , bot)
    match -> error $ show match

parseDest :: String -> Destination
parseDest str =
  let pat = "(bot|output) ([0-9]+)" in
  case str `fullMatch` pat of
    (_ : "bot" : destId : []) -> DestBot $ read destId
    (_ : "output" : destId : []) -> DestOut $ read destId
    match -> error $ show match

fullMatch :: String -> String -> [ String ]
str `fullMatch` pat
  | (match' : []) <- match = match'
  | otherwise = matchError match str
  where match = str =~ pat :: [[ String ]]

parseInput :: String -> Move
parseInput str =
  let pat = "value ([0-9]+) goes to ([a-z]+ [0-9]+)" in
  case str =~ pat :: [[String]] of
    (match : []) ->
      case match of
        (_ : val : botDest : []) ->
          (read val , parseDest botDest)
    match -> matchError match str 

matchError :: (Show a) => a -> String -> b
matchError match str = error $ "\n" ++ show match ++ " VS " ++ "\"" ++ str ++ "\""


test :: IO ()
test = do
  parsed <- Puzzle_10_Parser.readFile "input.txt"
  print parsed

-- main :: IO ()
-- main = test
