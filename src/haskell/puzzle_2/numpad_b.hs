module NumPad_B where

import NumPad

data B = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Num_A | Num_B | Num_C | Num_D
  deriving (Show, Read, Eq)

instance NumPad B where
  startingPoint = Five

  move L One = One
  move U One = One
  move R One = One
  move D One = Three

  move L Two = Two
  move U Two = Two
  move R Two = Three
  move D Two = Six

  move L Three = Two
  move U Three = One
  move R Three = Four
  move D Three = Seven

  move L Four = Three
  move U Four = Four
  move R Four = Four
  move D Four = Eight

  move L Five = Five
  move U Five = Five
  move R Five = Six
  move D Five = Five

  move L Six = Five
  move U Six = Two
  move R Six = Seven
  move D Six = Num_A

  move L Seven = Six
  move U Seven = Three
  move R Seven = Eight
  move D Seven = Num_B

  move L Eight = Seven
  move U Eight = Four
  move R Eight = Nine
  move D Eight = Num_C

  move L Nine = Eight
  move U Nine = Nine
  move R Nine = Nine
  move D Nine = Nine

  move L Num_A = Num_A
  move U Num_A = Six
  move R Num_A = Num_B
  move D Num_A = Num_A

  move L Num_B = Num_A
  move U Num_B = Seven
  move R Num_B = Num_C
  move D Num_B = Num_D

  move L Num_C = Num_B
  move U Num_C = Eight
  move R Num_C = Num_C
  move D Num_C = Num_C

  move L Num_D = Num_D
  move U Num_D = Num_B
  move R Num_D = Num_D
  move D Num_D = Num_D
