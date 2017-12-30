module Puzzle_11 where

import qualified Data.Set as S

{-
  The first floor contains a promethium generator and a promethium-compatible microchip.
  The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
  The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
  The fourth floor contains nothing relevant.
-}

data Rad = Promethium | Cobalt | Curium | Ruthenium | Plutonium
  deriving (Show, Eq, Ord)

type Gen = Rad
type Chip = Rad

type Gens = S.Set Gen
type Chips = S.Set Chip

data Floor = Floor
  { floorGens :: Gens
  , floorChips :: Chips
  } deriving (Show, Eq, Ord)

type Floors = [ Floor ]
type Env = Floors

puzzle =
  [ Floor
      (S.singleton Promethium)
      (S.singleton Promethium)
  , Floor
      (S.fromList [Cobalt, Curium, Ruthenium, Plutonium])
      (S.empty)
  , Floor
      (S.empty)
      (S.fromList [Cobalt, Curium, Ruthenium, Plutonium])
  , Floor
      (S.empty)
      (S.empty)
  ]

isValidEnv :: Env -> Bool
isValidEnv = all isValidFloor

isValidFloor :: Floor -> Bool
isValidFloor f
  | S.null $ floorGens f = True -- No generators, no radiation.
  | S.null $ floorChips f `S.difference` floorGens f = True -- No unattached chips.
  | otherwise = False

main :: IO ()
main = print "Yeah."
