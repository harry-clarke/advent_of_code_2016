module Puzzle_10_Solver where

import Puzzle_10_Parser
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State
import Control.Monad.Identity

-- data Env = Env Bots StartingMoves

type Updater m =
  Move -> Bot -> m (Bot , Moves)

type Listener m =
  Destination -> Bot -> Moves -> m ()


debug :: Listener IO
debug (DestBot 146) b m = print $ show "Debug: " ++ show b ++ "\n" ++ show m
debug _ _ _ = return ()

part1 :: Listener IO
part1 (DestBot id) _ ((17,_) : (61,_) : []) = print $ "Found: " ++ show id
part1 (DestBot id) _ ((61,_) : (17,_) : []) = print $ "Found: " ++ show id
part1 _ _ _ = return ()

updateBot :: Monad m => Listener m -> Updater m
updateBot l (val1', id) bot@Bot{ val1 = Nothing } = do
  let bot' = bot{ val1 = Just val1' }
  l id bot' [] >> return (bot' , [])
updateBot l (val1', id) bot@Bot{ val1 = Just val2' , val2 = Nothing } = do
  let (upper,lower) = if val1' > val2' then (val1', val2') else (val2', val1')
  let moves = [ (upper, upperId bot), (lower, lowerId bot)]
  let bot' = bot{ val1 = Nothing, val2 = Nothing }
  l id bot' moves >> return (bot', moves)

runMove :: Monad m => Updater m -> Move -> StateT Env m ()
runMove _ (_, DestOut _) = return ()
runMove u move@(_, DestBot id) = do
  (Env bots moves) <- get
  let bot = fromJust $ M.lookup id bots
  (bot', moves') <- lift $ u move bot
  put $ Env (M.insert id bot' bots) (moves' ++ moves)

runMove' :: Updater IO -> Move -> StateT Env IO ()
--runMove' u move = lift (print move) >> runMove u move
runMove' = runMove


run :: Monad m => Updater m -> StateT Env m ()
run u = do
  env <- get
  case env of
    (Env _ []) -> return ()
    (Env bots (m:ms)) -> do
      put (Env bots ms)
      runMove u m >> run u

run' :: Updater IO -> StateT Env IO ()
run' u = do
  env <- get
  case env of
    (Env _ []) -> return ()
    (Env bots (m:ms)) -> do
      put (Env bots ms)
      runMove' u m >> run' u

main :: IO ()
main = do
  let updater = run' (updateBot part1)
  env <- Puzzle_10_Parser.readFile "input.txt"
  env' <- execStateT updater env
  print "Done"
