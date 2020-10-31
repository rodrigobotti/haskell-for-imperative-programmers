module Parallel (
  parFib,
  bad,
  integralBadM,
  integralM
) where

import Data.List
import Control.DeepSeq
import Control.Parallel.Strategies
 
{-
DISCLAIMER

Parallel Haskell is experimental
It's API has changed and subject to further changes

---

Eval Monad

A computational unit
Can be parallel or sequential
Evaluation managed by the RTS

---

Strategies

type Strategy a = a -> Eval a

Makes the construction of computation composable!
Defines how to evaluate data

rpar :: Strategy a
rseq :: Strategy a
rdeepseq :: NFData a => Strategy a
runEval :: Eval a -> a
using :: a -> Strategy a -> a -- can be infix
                              -- x `using` s = runEval (s x)

---

runEval $ do
  a <- rpar (f x)
  b <- rpar (f y)
  return (a,b) -- happens before the computation complete -> lazy

runEval $ do
  a <- rpar (f x)
  b <- rseq (f y) -- forces b into WHNF i.e. `f y` thunk is evaluated
  return (a,b) -- happens after b

runEval $ do
  a <- rpar (f x) -- still parallel
  b <- rseq (f y)
  rseq a
  return (a,b) -- happens after a and b

runEval $ do
  a <- rpar (f x) -- still parallel
  b <- rpar (f y) -- still parallel
  rseq a
  rseq b
  return (a,b)

---

https://github.com/haskell/ThreadScope/releases -> mac requires `brew install gtk+`
https://wiki.haskell.org/ThreadScope_Tour/Spark
https://wiki.haskell.org/ThreadScope_Tour/SparkOverview
-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

parFib :: (Integer, Integer)
parFib = runEval $ do
  a <- rpar $ fib 35
  b <- rpar $ fib 36
  rseq a
  rseq b
  return (a, b)

bad :: IO ()
bad = do
  let xs = [1..1000000] :: [Integer]
  let x = force $ parMap rseq (+ 1) xs -- still bad: creates too many sparks
  -- let x = force $ parMap rpar (+1) xs -- wrong, parmap already parallelizes
  putStrLn $ show $ last $ x


integralBad :: (Ord a, Floating a) => (a -> a) -> a -> a -> a -> a
integralBad f step start end =
  if start >= end then 0
  else quad + rint
  where
    quad = (b-a) * f ((a+b)/2)
    a = start
    b = min (start+step) end
    rint = integral f step b end

f x = sin x + cos x + sinh x + cosh x

integralBadM :: IO ()
integralBadM = do
  putStrLn $ show $ integralBad f 0.01 0 (4*pi)

integral :: (Ord a, Floating a) => (a -> a) -> a -> a -> a -> a
integral f step start end = foldl' (+) 0 quads -- strict folding
  where
    -- area of each rectangle
    quads = map (\(a,b) -> (b-a) * f ((a+b)/2)) steps `using` parBuffer 100 rseq
    steps = stepList step start end

stepList :: (Ord a, Floating a) => a -> a -> a -> [(a,a)]
stepList steplength start end = -- produces the rectangle bases
  if start >= end then []
  else step : rest
  where
    step = (a, b)
    a = start
    b = min (start + steplength) end
    rest = stepList steplength b end

integralM :: IO ()
integralM = do
  putStrLn $ show $ integral f 0.000001 0 (4*pi)


{-
 -threaded \
 -rtsopts  \
 -eventlog \
 -with-rtsopts=-N \
 -with-rtsopts=-s # summary of the garbage collection
 -with-rtsopts=-ls # creates .eventlog file that can be analysed with threadscope
-}
