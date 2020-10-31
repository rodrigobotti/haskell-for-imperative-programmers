module Datatypes.Datatypes where

data Color =
  Red | Orange | Yellow | Green | Blue | Magenta

data PeaNum =
  Succ PeaNum | Zero

data Calculation 
  = Add Int Int
  | Sub Int Int
  | Mul Int Int
  | Div Int Int

data Tree a 
  = Leaf
  | Node (Tree a) a (Tree a)

calc :: Calculation -> Int
calc (Add x y) = x+y
calc (Sub x y) = x-y
calc (Mul x y) = x*y
calc (Div x y) = div x y

incr :: PeaNum -> PeaNum
incr = Succ

decr :: PeaNum -> PeaNum
decr (Succ n) = n

add :: PeaNum -> PeaNum -> PeaNum
add Zero n      = n
add (Succ m) n  = Succ $ add m n

sumP :: [PeaNum] -> PeaNum
sumP = foldr add Zero

data Person = Person { name :: String, 
                        age :: Int }
-- automaticaly generates
-- name :: Person -> String
-- age :: Person -> Int 

greet :: Person -> String
greet person = "Hi " ++ name person
-- greet (Person name _) = "Hi " ++ name

data Point 
  = D2 { x :: Int, y :: Int }
  | D3 { x :: Int, y :: Int, z :: Int }
-- z (D2 1 2) => *** Exception: No match in record selector z

-- type to create type aliases
type Forest a = [(Tree a)]

{- 
newtype can only be used with datatypes 
that have a single constructor with a single parameter.
the new type and the type of the field are in direct correspondence
(isomorphic).
new types are checked at compile time, yet ignored at runtime
(no work done when pattern matching).
-}
newtype Name = Name String