module Exercises.ExerciseList where

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ []     = False
myElem y (x:xs) =
  (x == y) || (myElem y xs)

myNub :: (Eq a) => [a] -> [a]
myNub []        = []
myNub (x:xs)
  | myElem x xs = myNub xs
  | otherwise   = x : myNub xs

isAsc :: [Integer] -> Bool
isAsc []        = True
isAsc [x]       = True
isAsc (x:y:xs)  =
  (y >= x) && isAsc (y:xs)

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] x y  = x == y
hasPath xs x y
  | x == y      = True
  | otherwise   =
    let xs' = [ (n,m) | (n,m) <- xs, n /= x ]
    in
      or [ hasPath xs' m y | (n, m) <- xs, n == x ]


  