module Day3.Math where

data Vect2D a = Vect2D a a deriving (Show, Eq)
zeroVect2D :: (Num a) => Vect2D a
zeroVect2D = Vect2D 0 0

class Vector2D a where
  toVector2D :: (Num b) => a -> Vect2D b

infixl 7 |+|
infixl 7 |-|
infixl 7 |*|
infixl 7 .*

(|+|) :: (Num a) => Vect2D a -> Vect2D a -> Vect2D a
(Vect2D ux uy) |+| (Vect2D vx vy) = Vect2D (ux + vx) (uy + vy)

(|-|) :: (Num a) => Vect2D a -> Vect2D a -> Vect2D a
u |-| v = u |+| vnegate v

(|*|) :: (Num a) => Vect2D a -> Vect2D a -> a
(Vect2D ux uy) |*| (Vect2D vx vy) = ux*vx + uy*vy

(.*) :: (Num a) => a -> Vect2D a -> Vect2D a
(.*) scl = fmap (scl *)

vectFromTuple :: (Num a) => (a, a) -> Vect2D a
vectFromTuple = uncurry Vect2D

vnegate :: (Num a) => Vect2D a -> Vect2D a
vnegate = fmap negate

instance Functor Vect2D where
  fmap fn (Vect2D x y) = Vect2D (fn x) (fn y)

instance Foldable Vect2D where
  foldr fn ini (Vect2D a b) = a `fn` (b `fn` ini)

manhattan :: (Num a) => Vect2D a -> Vect2D a -> a
manhattan u v = sum w
  where w = fmap abs (u |-| v)
