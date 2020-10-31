module Datatypes.EitherStuff where

import Data.Either

{-
data Either a b = Left a | Right b
lefts :: [Either a b] -> [a]
rights :: [Either a b] -> [b]
isLeft :: Either a b -> Bool
isRight :: Either a b -> Bool
fromLeft :: a -> Either a b -> a
fromRight :: b -> Either a b -> b
either :: (a -> c) -> (b -> c) -> Either a b -> c
partitionEithers :: [Either a b] -> ([a], [b])
-}
