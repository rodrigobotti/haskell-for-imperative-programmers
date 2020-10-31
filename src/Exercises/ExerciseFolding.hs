module Exercises.ExerciseFolding where

rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

prefixes :: [a] -> [[a]]
prefixes =
  foldr (\x acc -> [x] : (map ((:) x) acc)) []

lagrange :: [(Float, Float)] -> Float -> Float
lagrange xys x =
  foldl (\acc (xj, yj) -> acc + yj * lj xj) 0 xys
  where
    lj xj =
      foldl (
        \acc (xm, _) ->
          if xj==xm then 
            acc
          else 
            acc * ((x - xm)/(xj - xm))
      ) 1 xys

data Trie a = Leaf a | Node a [Trie a]

t =
  Node 'c' [
    Node 'a'
      [Leaf 'r', Leaf 't'],
    Node 'o'
      [Node 'o'
        [Leaf 'l']]]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f z (Leaf a)     = f z a
foldtrie f z (Node a [])  = f z a
foldtrie f z (Node a ts)  = 
  foldl (foldtrie f) (f z a) ts

{-
prefixes = 
  snd . foldl (\(upto, acc) x -> let xs = upto ++ [x] in (xs, acc ++ [xs]) ) ([], [])
rev = foldl (flip (:)) []
lagrange xys x =
  foldl (\acc (xj, yj) -> let term = yj * (lj xj x (map (fst) xys)) in acc + term) 0 xys

lj :: Float -> Float -> [Float] -> Float
lj xj x =
  foldl (
    \acc xm ->
      if xj==xm then 
        acc
      else 
        acc * ((x - xm)/(xj - xm))
  ) 1
-}