{-# LANGUAGE ScopedTypeVariables #-}
module Concurrency.Semaphores (hello) where

import System.IO
import Control.Concurrent
import Control.Exception

import Concurrency.Utils (forkN)

-- with this we see interleaved messages if each thread does not acquire a lock on stdout 
-- (race condition on shared resource)
disableStdOutBuffering :: IO ()
disableStdOutBuffering = 
  hSetBuffering stdout NoBuffering 

getGreeting :: IO String
getGreeting = do
  tid <- myThreadId
  let greeting = "Hello from " ++ show tid
  return $! greeting

threadHello :: QSem -> QSemN -> IO ()
threadHello mutex endFlags = do
  greeting <- getGreeting
  waitQSem mutex
  finally
    (putStrLn greeting)
    (signalQSem mutex)
  signalQSemN endFlags 1

hello :: IO ()
hello = do
  disableStdOutBuffering
  let n::Int = 10
  mutex     <- newQSem 0
  endFlags  <- newQSemN 0
  forkN n (threadHello mutex endFlags)
  signalQSem mutex
  waitQSemN endFlags n