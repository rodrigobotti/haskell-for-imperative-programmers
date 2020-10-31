module Exercises.ExerciseAdvanced (
  inv_tup_tree,
  cut,
  insert,
  newTree,
  inorder,
  testPropTree,
  task
) where

import System.IO
import Test.QuickCheck
import Data.List hiding (insert)
import Data.Char (toLower)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

inv_tup_tree :: Tree (Integer, Integer)
inv_tup_tree = buildNode 0 0
  where
    buildNode x y =
      Node (buildNode (x+1) y) (x, y) (buildNode x (y+1))

-- if height zero it is supposed to return empty tree
cut :: Integer -> Tree a -> Tree a
cut 0 tree = Leaf
cut _ Leaf = Leaf
cut level (Node tl a tr) =
  Node (cut (level-1) tl) a (cut (level-1) tr)

insert :: (Ord a) => a -> Tree a -> Tree a
insert a Leaf = Node Leaf a Leaf
insert a (Node tl b tr)
  | a <= b    = Node (insert a tl) b tr
  | otherwise = Node tl b (insert a tr)

newTree :: (Ord a) => [a] -> Tree a
newTree = foldl (flip insert) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node tl a tr) =
  (inorder tl) ++ [a] ++ (inorder tr)

propTree :: [Integer] -> Property
propTree xs =
  not (null xs) ==>
  (inorder $ newTree xs) == (sort xs)

testPropTree = quickCheck propTree

getWordsUntil :: String -> (IO [String])
getWordsUntil stop = aux []
  where
    aux acc = do
      _ <- putStr "> "
      v <- getLine 
      if v == "" then
        return acc
      else
        aux (acc ++ [v])

sToLower :: String -> String
sToLower = map toLower

containsIgnoreCase :: String -> String -> Bool
containsIgnoreCase str word =
  isInfixOf (sToLower word) (sToLower str)

search :: String -> [String] -> ([String], [String])
search text =
  foldl (doSearch) ([], [])
    where
      doSearch (isIn, notIn) word
        | containsIgnoreCase text word = ((isIn ++ [word]), notIn)
        | otherwise = (isIn, (notIn ++ [word]))

printWords :: String -> [String] -> IO ()
printWords sufix =
  foldl (\acc w -> acc >> putStrLn ("\"" ++ w ++ "\" " ++ sufix)) (return ())


task :: IO ()
task = do
  hSetBuffering stdout NoBuffering -- so haskell does not buffer stdout
  _       <- putStrLn "Specify words to search:"
  words   <- getWordsUntil ""
  _       <- putStrLn "File to search:"
  file    <- getLine
  content <- readFile file
  let (isIn, notIn) = search content words
  _       <- printWords "found" isIn
  _       <- printWords "NOT found" notIn
  return ()

{-

if height = 0 is supposed to return the whole tree
cut :: Integer -> Tree a -> Tree a
cut 0 tree = tree
cut _ Leaf = Leaf
cut height (Node tl a tr)
  | height == 1 = (Node Leaf a Leaf)
  | otherwise   = (Node (cut (height-1) tl) a (cut (height-1) tr))


printWords sufix = -- mapM_ (\w -> putStrLn ...)
-}