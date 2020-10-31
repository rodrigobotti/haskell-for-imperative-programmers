import Control.Monad

import Utils
import Datatypes.MonadStuff
import Tests
import Infinite as I
import qualified Program as P

import qualified Exercises.ExerciseList as E1
import qualified Exercises.ExerciseFolding as E2

inRange :: Integer -> Integer -> Integer -> Bool
inRange min max x = 
  x >= min && x <= max

fac :: Integer -> Integer
fac n = aux n 1
  where 
    aux n acc 
      | n <= 1    = acc
      | otherwise = aux (n - 1) (n * acc)

asc :: Integer -> Integer -> [Integer]
asc n m
  | m < n   = []
  | m == n  = [m]
  | m > n   = n : asc (n + 1) m

mySum :: Num a => [a] -> a
mySum []      = 0
mySum (x:xs)  = x + sum xs

evens :: [Integer] -> [Integer]
evens []          = []
evens (x:xs)
  | mod x 2 == 0  = x : evens xs
  | otherwise     = evens xs

addTuples :: [(Integer, Integer)] -> [Integer]
addTuples xs = [ x+y | (x,y) <- xs ]

addTuples2 :: [(Integer, Integer)] -> [Integer]
addTuples2 = map (\(x,y) -> x + y)

evens2 :: [Integer] -> [Integer]
evens2 = filter (\x -> x `mod` 2 == 0)

map2D :: (a -> b) -> [[a]] -> [[b]]
map2D = map . map

count :: Eq a => a -> [a] -> Integer
count e =
  foldr (\x acc -> if e == x then acc + 1 else acc) 0

myLength :: [a] -> Integer
myLength = 
  foldr (const $ (+) 1) 0

myMap :: (a -> b) -> [a] -> [b]
myMap f = 
  foldr ((:) . f) []

--

main :: IO ()
main =
  printAll
    [
      "Advanced Excercises"
      , show $ E3.cut 3 E3.inv_tup_tree
      , show $ E3.inorder $ E3.cut 3 E3.inv_tup_tree
      , show $ E3.inorder $ E3.newTree [4, 1, 2, 3, 9]
    ] 
  >> E3.testPropTree
  >> E3.task
  >>  
  printAll
    [
      take 5 I.ones
      , take 5 I.evens
      , take 5 I.odds
      , take 50 I.fibs
    ]
  >>  
  printAll 
    [
    monadd (Just 1) (Just 2)
    , monadd Nothing (Just 3)
    , Nothing >> Just 1
    ]
  >>
  printAll 
    [
    safediv 10 5
    , safediv 1 0
    ]
  {- 
  printAll
    [ "Exercise 2 - Folding"
    , show $ E2.rev [1..10]
    , show $ E2.prefixes [1..5]
    , show $ (E2.foldtrie (\acc x -> [x] ++ acc) [] E2.t)
    ]
  >>
  putStrLn $ show $ mySum2 [1..10]
  >>
  printAll
    [ show $ addTuples2 [(x,y) | x <- [1..3], y <- [1..3]] 
    , show $ evens2 [1..20] 
    ]
   -}  
   {- printAll
    [ "Exercise 1 - Lists"
    , show $ E1.isAsc [1..10]
    , show $ E1.isAsc [1, 2, 3, 5, 4]
    , show $ E1.myNub [1, 2, 1, 3, 3, 4, 5]
    , show $ E1.myElem 10 [1..5] 
    ] -}
  {- >> 
  printAll
    [ show $ mySum [1..10]
    , show $ evens [1..20] 
    , show $ addTuples [(x,y) | x <- [1..3], y <- [1..3]] 
    ]
  >>
  printAll 
    [ show $ inRange 0 10 5
    , show $ fac 50
    , show $ asc 1 15
    , show [2*x | x <- [1,2,3,4], x > 1]
    , show $ [(x,y) | x <- [1,2,3], y <- ['a', 'b']]
    , "😀" 
    ]
 -}