module Concurrency.Utils where

import Control.Concurrent

forkN :: Int -> IO () -> IO ()
forkN n action =
  mapM_ (const $ forkIO $ action) [1..n]