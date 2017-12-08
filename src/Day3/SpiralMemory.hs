module Day3.SpiralMemory where

-- Iterative Cursor-based Approach, a lot of the complexity is isolated in Cursor.hs

import Day3.UlamCursor
import Day3.Math

spiralDistanceFromOrigin :: Int -> Maybe Int
spiralDistanceFromOrigin n
  | n >= 1 =
    Just $ case spiralFind n of
            Just v -> manhattan zeroVect2D v
            Nothing -> 0
  | otherwise = Nothing

spiralFind :: Int -> Maybe (Vect2D Int)
spiralFind 0 = Nothing
spiralFind n = Just $ last $ steps initialUlamCursor
    where getPosition (UlamCursor pos _) = pos
          stepSequence = map getPosition . iterate transition
          steps = take n . stepSequence

-- Explicit Form -- O(1)

{-
  For the sake of study, examine:

  17 16 15 14 13
  18  5  4  3 12
  19  6  1  2 11
  20  7  8  9 10
  21 22 23 24 25 ...

  Suppose we are looking for spiralSteps(20):

  Let's view the spiral from n = 1..25 as a series of 3 nested squares. The first square being
  the sqaure encapsulating 1, the second encapsulating 1..9 and the third encapsulating 1..25.

  Imagine walking from the center-out in a spiral, counting up for every step we take. When we enter a new square,
  start at 1. When we reach a corner, reset to 0. That is, you get the sequence:

  1, 0, 1, 0, 1, 0, 1, 0
  or
  1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0

  for i = 2..9 and i = 10..25, respectively (At step=0, the cornerOffset = 0). These values represent the distance from the corners
  of their most immediate, encapsulating square for any value (i.e. the one they are on the perimeter of). Call this value the
  cornerOffset of x.

  If we now imagine walking in an ordinal direction from the center, straight out, until we enter the square
  containing 20, we end up at one of 11, 15, 19, or 23. All of those numbers have the cornerOffset of 2. This value
  is the axisOffset for that square such that axisOffset(x) = axisOffset(Square) for all x in Perimeter(Square).

  Since the distance from the center is the manhattan (or taxicab) distance, we can choose the path consisting
  of the least turns, that is traversing to the nearest axis location (|axisOffset - cornerOffset|) and then
  traversing along the axis to the center (axisOffset), yielding:

  sprialDist(n) = axisOffset(n) + |axisOffset(n) - cornerOffset(n)|

  In compressed numerical form:

  if:
  axisOffset(n) = floor(ceil(sqrt(n)) / 2)  <-- Essentially, SquareSideLength/2 rounded down to match offset
  cornerOffset(n) = (n - (2u-1)^2) mod 2u   <-- Note, as above, 2u = (SquareSideLength - 1 for the corner) because of the use of floor.
    where u = axisOffset(n)                    | n - (2u-1)^2 strips out the internal square contents, leaving the perimeter of the square we care about
  Then:
  spiralSteps(n) = axisOffset(n) + |cornerOffset(n) - axisOffset(n)|
-}

isqrt :: (Integral a, Floating b) => a -> b
isqrt = sqrt . fromIntegral

axisOffset :: Int -> Int
axisOffset n = floor $ (fromIntegral . ceiling . isqrt) n / 2.0

cornerOffset :: Int -> Int
cornerOffset n =  rem (n - ((2 * aoff - 1) ^ 2)) $ 2 * aoff
  where aoff = axisOffset n

spiralSteps :: Int -> Maybe Int
spiralSteps n
    | n >= 2 = Just (aoff + abs (off - aoff))
    | n == 1 = Just 0
    | otherwise = Nothing
      where aoff = axisOffset n
            off = cornerOffset n

{-
  Part 2 is currently incomplete just becuase of how painful it is. There is no easy way to adapt the
  Part 1 solution to this case and there is no clear explicit form (at least not from myself or others I've seen).

  The solution I used was admittedly taken from OEIS, but let's be honest. Kruft is annoying.
-}
