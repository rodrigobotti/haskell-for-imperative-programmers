module Datatypes.MonadStuff where

{-
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {- # MINIMAL (>>=) # -}
-}

monadd :: (Monad m, Num a) => m a -> m a -> m a
monadd mx my = do
  x <- mx
  y <- my
  return $ x+y

{-
monadd mx my =
  mx >>= (x -> my >>= (\y -> return $ x+y))

m >>= (\x -> ...)
===
do
  x <- m

instance Monad Maybe where
  m >>= f = case m of
              Nothing -> Nothing
              Just x -> f x
  return v = Just v
-}

{-
Monad Laws

Left identity:
  return a >>= k = k a

Right identity:
  m >>= return = m

Associativity:
  m >>= (\x -> k x >>= h) = (m >>= k) >>= h

-}