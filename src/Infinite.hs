module Infinite where

ones :: [Integer]
ones = 1 : ones

nat :: [Integer]
nat = asc 1
  where asc n = n : (asc $ n+1)

evens :: [Integer]
evens = map (*2) nat

odds :: [Integer]
odds = filter (\x -> x `mod` 2 /= 0) nat

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{-
We can:
- transform (map, filter, list comprehensions)
- take and drop (use pattern matching)
- build new lists from infinite lists

We cannot:
- evaluate the whole list
- evaluate the end
-}