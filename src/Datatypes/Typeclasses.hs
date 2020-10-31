module Datatypes.Typeclasses where
-- (+) :: Num a => a -> a -> a
-- `a` has to have an `instance` of the `Num` `typeclass`
{-
:info Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

:info Show
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {- # MINIMAL showsPrec | show # -} 
    ~> create either showsPred or show on instances, all other are derived

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {- # MINIMAL (==) | (/=) # -}

:info Ord
class Eq a => Ord a where ~> to be in Ord, also need to be in Eq
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
-}

data Temperature = C Float | F Float
  deriving Show
  {- 
    deriving (Show, Eq) 
    Eq's (==) is defined incorrectly by haskell
    creates structural equivalences e.g. (==) (C n) (F m) = n == m
    which might be ok for other datastructures (e.g. lists)
  -}

instance Eq Temperature where
  (==) (C n) (C m) = n == m
  (==) (F n) (F m) = n == m
  (==) (C c) (F f) = (1.8*c + 32) == f 
  (==) (F f) (C c) = (1.8*c + 32) == f
  -- could lead to rounding issues, but thats not the point
