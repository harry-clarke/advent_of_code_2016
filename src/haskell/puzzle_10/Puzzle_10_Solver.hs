module Puzzle_10_Solver where

import Puzzle_10_Parser
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.Monad.Identity

-- data Env = Env Bots StartingMoves

type Updater m =
  Move -> Bot -> m (Bot , Moves)

updateBot :: Monad m => Updater m
updateBot (val1', _) bot@Bot{ val1 = Nothing }=
  return (bot{ val1 = Just val1' }, [])
updateBot (val1', _) bot@Bot{ val1 = Just val2' , val2 = Nothing } = do
  let (upper,lower) = if val1' > val2' then (val1', val2') else (val2', val1')
  let moves = [ (upper, upperId bot), (lower, lowerId bot)]
  return (bot{ val1 = Nothing, val2 = Nothing }, moves)

runMove :: Monad m => Move -> StateT Env m ()
runMove (_, DestOut _) = return ()
runMove move@(_, DestBot id) = do
  (Env bots moves) <- get
  let bot = fromJust $ M.lookup id bots
  (bot', moves') <- updateBot move bot
  put $ Env (M.insert id bot bots) (moves' ++ moves)

run :: Monad m => StateT Env m ()
run = do
  env <- get
  case env of
    (Env _ []) -> return ()
    (Env bots (m:ms)) -> do
      put (Env bots ms)
      runMove m >> run

main :: IO ()
main = do
  env <- Puzzle_10_Parser.readFile "input.txt"
  env' <- execStateT run env
  print env'
