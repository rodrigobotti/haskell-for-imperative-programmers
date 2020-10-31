import Control.Monad

import Utils

inRange :: Integer -> Integer -> Integer -> Bool
inRange min max x = 
  x >= min && x <= max

fac :: Integer -> Integer
fac n = aux n 1
  where 
    aux n acc 
      | n <= 1    = acc
      | otherwise = aux (n - 1) (n * acc)

--

main :: IO ()
main = 
  printAll 
    [ show $ inRange 0 10 5
    , show $ fac 50
    , "😀" 
    ]
