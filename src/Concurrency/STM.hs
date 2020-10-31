module Concurrency.STM (count) where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM 
-- had to add stm do cabal `build-depends`

-- (<Sum>, <Number of finished transactions>)
type Result = TVar (Int, Int)

addToResult :: Result -> Int -> STM ()
addToResult result x = do
  (sum, endCtr) <- readTVar result
  writeTVar result (sum+x, endCtr+1)

waitForCounter :: Result -> Int -> STM Int
waitForCounter result limit = do
  (sum, endCtr) <- readTVar result
  if endCtr < limit then retry 
  else return sum
 

count :: IO ()
count = do
  -- Number of threads to spawn
  let n = 100
  -- Set up TVar
  result <- atomically $ newTVar (0, 0)
  -- Spawn threads
  mapM_ (\x -> forkIO $ atomically $ addToResult result x) [1..n]
  -- Wait for threads to dinish and get sum
  sum <- atomically $ waitForCounter result n
  -- Print sum
  putStrLn $ "Sum [1..n] " ++ show sum
