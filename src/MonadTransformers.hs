module MonadTransformers where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State

-- naive approach

type IOMaybe a = IO (Maybe a)

returnIOM :: a -> IOMaybe a
returnIOM = return . Just

bindIOM :: IOMaybe a -> (a -> IOMaybe b) -> IOMaybe b
bindIOM iom f = do
  maybeVal <- iom
  case maybeVal of
    Nothing   -> return Nothing
    (Just v)  -> f v

(>>>=) = bindIOM

liftIOM :: IO a -> IOMaybe a
liftIOM io = io >>= returnIOM

checkInput :: String -> Bool
checkInput []     = False
checkInput (x:_)  = isUpper x

getNameN :: IOMaybe String
getNameN = do
  input <- getLine
  if checkInput input
  then returnIOM input
  else return Nothing

exampleN = putStr "Please enter your name: "
          >> getNameN
          >>>= (\s -> liftIOM $ putStrLn $ "Your name is " ++ s)

-- Transformers approach
-- is generic and allows stacking transformers

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
  return = MaybeT . return . Just

  x >>= f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT (f y)

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

getName :: MaybeT IO String
getName = do
  input <- lift getLine
  guard (checkInput input)
  return input

example = runMaybeT $ do
  lift $ putStr "Please enter your name: "
  name <- getName
  lift $ putStrLn $ "Your name is " ++ name

-- State Transformer
{-
newtype StateT s (m :: * -> *) a
runStateT :: StateT s m a -> s -> m (a,s)
evalStateT :: Monad m => StateT s m a -> s -> m s
execStateT :: Monadn m => StateT s m a -> s -> m s
get :: m s
put :: s -> m ()
-- (<Output>, <State>)
-}

readUntilWithState :: String -> IO Int
readUntilWithState ending = execStateT (aux ending) 0
  where aux ending = do
    count <- get
    input <- liftIO getLine
    put (count + 1)
    if input == ending 
    then return () 
    else aux ending

{-
StateMonad

type State s = StateT s Identity
runState :: State s a -> s -> (a,s)
evalState :: State s a -> s -> a
-}

{-
Useful Transformers

------------------------------------------------------------
Precursor | Transfomer  | Original      | Combined
          |             | Type          | Type
------------------------------------------------------------
Writer    | WriterT     | (a,w)         | m (a, w)
------------------------------------------------------------
Reader    | ReaderT     | r -> a        | r -> m a
------------------------------------------------------------
State     | StateT      | s -> (a, s)   | s -> m (a, s)
------------------------------------------------------------
Cont      | ContT       | (a -> r) -> r | (a -> m r) -> m r
------------------------------------------------------------
-}

{-
DISCLAIMER

Monad transformers are not finalized.
List transformer is deprecated.
The way we interface with them will change:
  Each monad transformer comes with an operation 
  run<name>T to unwrap the transformer, exposing a
  computation of the inner monad.
  Currently these functions are defined as field labels,
  but in the next major release they will be 
  separate functions.

-}
 