# Arrows

```hs
import Control.Arrow

class Control.Category.Category a => Arrow (a :: * -> * -> *) where
  arr :: (b -> c) -> a b c
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  (&&&) :: a b c -> a b c' -> a b (c, c')
  {-# MINIMAL arr, (first | (***)) #-}

-- arr is known as the "pirate function" LOL

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g . f

instance Arrow (->) where
  arr = id
  (***) f g (x,y) = (f x, g x)
```

```
a :: (Arrow a, Num c) => a c c
a = arr (+1)

b :: (Arrow a, Num c) => a c c
b = arr (*2)

comb :: (Arrow a, Num c) => a (c, c) c
comb = arr (\(x, y) -> x+y) -- (= uncurry (+))

c :: (Arrow cat, Num c) => cat c c
c = a &&& b >>> comb

          a ->
        /      \
c =  ->         comb -> 
        \      /
          b -> 


            (+1) -> 6
          /            \
c 5 =  ->               x+y -> 16
          \            /
            (*2) -> 10

```

## Kleisli Arrows

We cant do `getLine >>> putStrLn`

```hs
newtype Kleisli (m :: * -> *) a b
  = Kleisli {runKleisli :: a -> m b}

instance Monad m => Arrow (Kleisli m) where
  arr f = Kleisli (return . f)
  first (Kleisli f) = Kleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
  second (Kleisli f) = Kleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))

printInput = runKleisli ka ()
  where
    ka = Kleisli (\_ -> getLine) 
        >>> arr (map toUpper) -- create flows with monads
        >>> Kleisli putStrLn
```

## Choice Arrows

How to do flow control

```hs
class Arrow a => ArrowChoice (a :: * -> * -> *) where
  left :: a b c -> a (Either b d) (Either c d)
  right :: a b c -> a (Either d b) (Either d c)
  (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
  (|||) :: a b d -> a c d -> a (Either b c) d
  {-# MINIMAL (left | (+++)) #-}
```

```hs
listCase :: [a] -> Either () (a, [a])
listCase []     = Left ()
listCase (x:xs) = Right (x, xs)

mapA :: ArrowChoice a => a a1 a2 -> a [a1] [a2]
mapA f =
  arr listCase >>>
  arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))
``` 

## Arrow Application

Higher-order arrows

```hs
class Arrow a => ArrowApply (a :: * -> * -> *) where
  app :: a (a b c, b) c
  {-# MINIMAL app #-}

instance ArrowApply (->) where
  app (f,x) = f x

instance Monad m => ArrowApply (Kleisli m) where
  app = Kleisli (\(Kleisli f, x) -> f x)
```

## Looping Arrows

```hs
class Arrow a => ArrowLoop (a :: * -> * -> *) where
  loop :: a (b, d) (c, d) -> a b c
  {-# MINIMAL loop #-}

instance ArrowLoop (->) where
  loop f b = let (c,d) = f (f,d) in c

instance MonadFix m => ArrowLoop (Kleisli m) where
  loop (Kleisli f) = Kleisli (liftM fst . mfix . f')
    where
      f' x y = f (x, snd y)
```

## Wrapping up

Arrows are all about composition and dataflow!

**Point-Free Style**

```hs
delete w =
  arr words >>>
  arr (filter (==w)) >>>
  arr unwords
```

[Programming with Arrows](http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf)


