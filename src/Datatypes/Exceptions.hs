{-# LANGUAGE ScopedTypeVariables #-}
module Datatypes.Exceptions where

import Control.Exception

data MyError = ErrorA | ErrorB 
  deriving Show

instance Exception MyError

-- throw ErrorA
{- 
  Exceptions may be thrown from purely functional code, 
  but may only be caught with the IO monad.

  catch :: Exception e => IO a -> (e -> IO a) -> IO a
-}

failing :: IO ()
failing = do
  throw ErrorA

catching :: IO ()
catching = do
  catch (failing) (\(e :: MyError) -> putStrLn "Aaah shiiieeeet!") -- requires ScopedTypeVariables
{-
catching = do
  catch failing (\e -> do
    -- let t = (e :: SomeException) -- BAD: catches all, including user interrupts
    let t = (e :: MyError)
    putStrLn "Aaah shiiieeeet!"
    )
-}

{-
catching multiple

f = <expr>  `catch` \(ex :: ArithException) -> handleArith  ex
            `catch` \(ex :: IOException)    -> handleIO     ex
-- BAD: this construct makes it so each handler can catch exception thrown by previous handlers

-- GOOD:
f = <expr> `catches` [Handler (\(ex :: ArithException) -> handleArith  ex),
                      Handler (\(ex :: IOException)    -> handleIO     ex)]

-}

{-
Functions

try :: Exception e => IO a -> IO (Either e a)
tryJust :: Exception e => (e -> Maybe b) -> IO a -> IO (Either b a)
finally :: IO a -> IO b -> IO a
-}

{-
Are exceptions a good idea?

Purely funcitonal code has no concept of exceptions (can only be handled by IO actions)
Maybe and Either are purely functional.

But only exceptions can handle IO, Thread, System errors
-}


{-
Exception typeclass

$> :i Exception

class (base-4.12.0.0:Data.Typeable.Internal.Typeable e, Show e) =>
      Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String
  	-- Defined in `GHC.Exception.Type'
instance Exception SomeAsyncException
  -- Defined in `GHC.IO.Exception'
instance Exception IOException -- Defined in `GHC.IO.Exception'
instance Exception Deadlock -- Defined in `GHC.IO.Exception'
instance Exception CompactionFailed
  -- Defined in `GHC.IO.Exception'
instance Exception BlockedIndefinitelyOnSTM
  -- Defined in `GHC.IO.Exception'
instance Exception BlockedIndefinitelyOnMVar
  -- Defined in `GHC.IO.Exception'
instance Exception AsyncException -- Defined in `GHC.IO.Exception'
instance Exception AssertionFailed -- Defined in `GHC.IO.Exception'
instance Exception ArrayException -- Defined in `GHC.IO.Exception'
instance Exception AllocationLimitExceeded
  -- Defined in `GHC.IO.Exception'
instance Exception SomeException -- Defined in `GHC.Exception.Type'
instance Exception ArithException
  -- Defined in `GHC.Exception.Type'
instance Exception ErrorCall -- Defined in `GHC.Exception'
instance Exception TypeError -- Defined in `Control.Exception.Base'
instance Exception RecUpdError
  -- Defined in `Control.Exception.Base'
instance Exception RecSelError
  -- Defined in `Control.Exception.Base'
instance Exception RecConError
  -- Defined in `Control.Exception.Base'
instance Exception PatternMatchFail
  -- Defined in `Control.Exception.Base'
instance Exception NonTermination
  -- Defined in `Control.Exception.Base'
instance Exception NoMethodError
  -- Defined in `Control.Exception.Base'
instance Exception NestedAtomically
  -- Defined in `Control.Exception.Base'
-}