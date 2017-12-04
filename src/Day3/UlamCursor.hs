{-# LANGUAGE FlexibleInstances #-}

module Day3.UlamCursor (UlamCursor(..), OrdinalDirection(..), Position, Level, Counter, initialUlamCursor, transition) where

import Day3.Math
data OrdinalDirection = North | South | East | West deriving (Show, Eq)

type Position = Vect2D Int
type Level = Int
type Counter = Int

data Movement = Movement Level Counter OrdinalDirection deriving (Show, Eq)
data UlamCursor = UlamCursor Position Movement deriving (Show, Eq)

initialUlamCursor :: UlamCursor
initialUlamCursor = UlamCursor zeroVect2D (Movement 0 0 East)

movementDirection :: Movement -> OrdinalDirection
movementDirection (Movement _ _ d) = d

class StateMachine a where
  transition :: a -> a

instance Vector2D OrdinalDirection where
  toVector2D North = vectFromTuple (0, 1)
  toVector2D West  = vectFromTuple (-1, 0)
  toVector2D South = vectFromTuple (0, -1)
  toVector2D East  = vectFromTuple (1, 0)

instance StateMachine OrdinalDirection where
  transition North = West
  transition West = South
  transition South = East
  transition East = North

instance StateMachine Movement where
  transition (Movement level count dir)
    | count /= 0 = Movement level (count - 1) dir
    | count == 0 =
      let isIncrementPoint = dir `elem` [North, South]
          newLevel = if isIncrementPoint then level + 1 else level
      in Movement newLevel newLevel (transition dir)

instance StateMachine UlamCursor where
  transition (UlamCursor u mvmt) =
    let du = toVector2D $ movementDirection mvmt
    in UlamCursor (u |+| du) (transition mvmt)
