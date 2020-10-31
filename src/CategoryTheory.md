# Categories

Higher abstraction than algebras.
"Objects" and "Arrows"
![](https://upload.wikimedia.org/wikipedia/commons/thumb/f/ff/Category_SVG.svg/440px-Category_SVG.svg.png)
  
  1A, 1B, 1C are identity morphisms
  g . f is a composition

Given a category **C**
```
obj(C) := Class of objects
hom(C) := Class of morphisms
C(a,b) := All morphisms from a to b
```
Composition of morphisms **.**
```
h . f . g = (h.f).g = h.(f.g)
f.1 = 1.f = f
```
Identity morphisms for each object

**Examples**
- Subset P of natural numbers with <= operator
- Type system of haskell
- Natural numbers with adition of natural constants
- Set, the category of sets
  ```
  obj(Set) := All sets
  hom(Set) := Total functions
  Set(a,b) := Every total function from a to b
  ```

Algebras are not categories! or are they?
  Monoids can be interpreted as categories: 
  Every "object" can be represented as a composition of "adders" (mappend)


## Morphisms between categories

### Functors

Mapping morphisms from one category to another (types?)

```hs
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}

instance Functor [] where
  fmap = map
```

### Monoidal Categories
```
Category C, Functor (+)
(+) : C x C -> C (tensor product)
I   : Identity object in obj(C)
Three natural transformations: 
  alpha, lambda and rho
  follow coherence conditions
```

...can't write anything that will do justice to the video...

### Monoidal Functors

...can't write anything that will do justice to the video...

```hs
class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a,b)

instance Monoidal [] where
  unit        = [()]
  (**) as bs  = [(a,b) | a <- as, b <- bs]

(<**>) :: Monoidal f => f (a -> b) -> f a -> f b
(<**>) mf mx = fmap (\(f, x) -> f x) (mf ** mx)

lift2 :: (a -> b -> c) -> (f a -> f b -> f c)
lift2 f x = (<**>) (fmap f x)

lift3 :: (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
lift3 f a b c = lift2 f a b <**> c

lift<n> f x1 ... xn = lift<n-1> f x1 ... xn-1 <**> xn
```

### Applicatives
  
Applicatives are equivalent to (lax) monoidal functors!

```hs
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)
  {-# MINIMAL pure, ((<*>) | liftA2) #-}

putStrLn <$> ((++) <$> getLine <*> getLine)

instance Applicative IO where
  pure = return
  a <*> b = do
    f <- a
    b <- b
    return (f x)
```

### Monoids
...can't write anything that will do justice to the video...

### Monads
...can't write anything that will do justice to the video... 

```hs
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=) #-}
```

Monads are Applicative Functors in haskell due to their property to not leave their context

## Laws
### Functors
```hs
fmap id = id                    -- Identity
fmap (f . g) = fmap f . fmap g  -- Composition
```

### Applicatives:
```hs
pure id <*> v = v                             -- Identity
pure f <*> pure x = pure (f x)                -- Homomorphism
u <*> pure y = pure ($ y) <*> u               -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)  -- Composition
```

### Monads:
```hs
return a >>= f = f a                      -- Left Identity
m >>= return = m                          -- Right Identity
(m >>= f) >>= g = m >>= (\x -> f x >>= g) -- Associativity
```

## Where to go from here:
- [Programming with categories](https://www.youtube.com/playlist?list=PLhgq-BqyZ7i7MTGhUROZy3BOICnVixETS)
- [Category Theory for Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf)
- [Seven sketches of compositionality](https://arxiv.org/pdf/1803.05316.pdf)
- [Applicative programming with effects](http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf)
