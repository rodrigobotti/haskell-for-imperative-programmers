module Concurrency.MVarsAndChans (hello) where

import System.IO
import Control.Concurrent
import Control.Exception

import Concurrency.Utils (forkN)

{-
Concurrent Haskell is a language extensions that comes applied by default.
It is not standard haskell98 though.

Concurrent Haskell uses green threads.
-}

{-
Creating threads

forkIO :: IO () -> IO ThreadId
-- executes the action in a new thread
-}

{-
How threads comunicate

IO producing functions can receive parameters.

---
Shared memory using MVar.
Actions on MVars are atomic.

newEmtpyMVar :: IO (MVar a)
newMVar :: a -> IO (MVar a)
takeMVar :: MVar a -> IO a -- atomic & blocking
putMVar :: MVar a -> a -> IO () -- atomic & blocking

---
CSP Channels using Chan

Channels are shared unbounded FIFO queues implemented with MVars.

newChan :: IO (Chan a)
writeChan :: Chan a -> a -> IO ()
readChan :: Chan a -> IO a
-}

f :: Int -> Int -> MVar Int -> IO ()
f a b mVar =
  -- putMVar mVar (a+b) -- due to lazy evaluation this only stores the thunk a+b -> would evaluate the computation in the thread that reads the result
  putMVar mVar $! (a+b) -- this strictly evaluates in the forked thread

t :: IO ()
t = do
  mVar    <- newEmptyMVar
  forkIO $ f 1 2 mVar
  result  <- takeMVar mVar
  putStrLn $ show result

getGreeting :: IO String
getGreeting = do
  tid <- myThreadId
  let greeting = "Hello from " ++ show tid
  return $! greeting


---

type Mutex = MVar ()
type Event = Chan ()

acquire :: Mutex -> IO ()
acquire mutex = takeMVar mutex

release :: Mutex -> IO ()
release mutex = putMVar mutex ()

newMutex :: IO Mutex
newMutex = newEmptyMVar

newEvent :: IO Event
newEvent = newChan

signal :: Event -> IO ()
signal e = writeChan e ()

-- blocks untill we've read n items from channel
waitSignals :: Int -> Event -> IO ()
waitSignals n e =
  mapM_ (const $ readChan e) [1..n]

-- with this we see interleaved messages if each thread does not acquire a lock on stdout 
-- (race condition on shared resource)
disableStdOutBuffering :: IO ()
disableStdOutBuffering = 
  hSetBuffering stdout NoBuffering 

threadHello :: Mutex -> Event -> IO ()
threadHello mutex endFlags = do
  greeting <- getGreeting
  
  acquire mutex -- "lock" shared resource
  finally
    (putStrLn greeting)
    (release mutex) -- release resource
  
  signal endFlags -- signal ending

hello :: IO ()
hello = do
  disableStdOutBuffering
  let n = 10
  mutex     <- newMutex
  endFlags  <- newEvent
  -- fire threads
  forkN n (threadHello mutex endFlags)
  release mutex
  -- wait for threads
  waitSignals n endFlags

{-
had to add `ghc-options: -threaded` in cabal
had to add `ghc-options: -rtsopts=-N` so the RTs can infer the number of processors

they are equivalent to:
$> ghc -threaded <main>.hs -o <main>
$> ./<main> +RTS -N
-}