module Puzzle_10_Solver where

import Puzzle_10_Parser
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State

-- data Env = Env Bots StartingMoves

updateBot :: Move -> Bot -> (Bot, Moves)
updateBot (val1', _) bot@Bot{ val1 = Nothing }=
  (bot{ val1 = Just val1' }, [])
updateBot (val1', _) bot@Bot{ val1 = Just val2' , val2 = Nothing } =
  let (upper,lower) = if val1' > val2' then (val1', val2') else (val2', val1') in
  let moves = [ (upper, upperId bot), (lower, lowerId bot)] in
  (bot{ val1 = Nothing, val2 = Nothing }, moves)

runMove :: Move -> Env -> Env
runMove (_, DestOut _) env = env
runMove move@(_, DestBot id) (Env bots moves) =
  let bot = fromJust $ M.lookup id bots in
  let (bot', moves') = updateBot move bot in
  Env (M.insert id bot bots) (moves' ++ moves)
  


run :: Env -> Env
run env@(Env _ []) = env
run env@(Env bots (m:ms)) = run $ runMove m (Env bots ms)

main :: IO ()
main = do
  env <- Puzzle_10_Parser.readFile "input.txt"
  print $ run env
