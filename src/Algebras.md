# ALGEBRA 

Consists of a domain and operations on that domain.
Operations have properties.

---

## Examples


### Natural Number Addition

**Domain**: Natural Numbers
**Operations**: Addition
**Properties**: Associativity
```
21+(1+2)≡(21+1)+2
```

**Formalization**:
**Domain**: ℕ
**Operation**: 
```hs
+ :: ℕ -> ℕ -> ℕ
```
**Property**: `(a+b)+c ≡ a+(b+c)`

### "Nothing"-Algebra

Domain: {∅}
Operation: None
Property: None

### "School Arithmetic"-Algebra

**Domain**: `ℝ`
**Operation**: 
```hs
+,-,*,/ :: ℝ -> ℝ -> ℝ
```
**Properties**: Associativity and commutativity for + and *

### "Perfect Hash"-Algebra

**Domain**: 
```
B = {w | w ∈ U[i=1;n] {0,1}^i } 
-- all binary strings of length 1 to n
```
**Operation**: 
```hs
h :: B -> B
```
**Property**: 
```
∀ x,y ∈ B . x!=y => h(x)!=h(y)
```

## MAGMA

A set S with a closed binary operation
The operation produces elements that are in S
(Note: Domain=S, Operation=the closed binary operation)


### Semigroup

An associative magma
```
(a . b) . c === a . (b . c)
```

```hs
class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup [a] where
  (<>) = (++)

instance Semigroup (Either a b) where
  Left _  <> b = b
  a       <> _ = a
  -- left-most Right constructor
```

### Monoid

A semigroup with an identity element
```
(a . b) . c === a . (b . c)
I . a === a === a . I
```

#### Monoid Laws (Properties)

**Right Identity**:
```hs
x <> mempty = x
```

**Left identity**:
```hs
mempty <> x = x
```

**Associativity**:
```hs
x <> (y <> z) = (x <> y) <> z
```

#### Concatenation:
```hs
mconcat = foldr (<>) mempty

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a -- can define a new one but it can be (<>)
  mconcat :: [a] -> a
  # MINIMAL mempty #

instance Monoid [a] where
  mempty      = []
  mconcat xss = [x | <- xss, x <- xs]
```

#### Problem
```hs
instance Monoid Int where
  ?
```
Not every type has *one* monoid!
What is the associative operation for numeric types? both (+) and (*)
    
**Trick**:
```hs
newtype Sum a = Sum { getSum :: a } 
  deriving (Eq, Ord, ...)

instance Num a => Semigroup (Sum a) where
  (<>) = coerce ((+) :: a -> a -> a)
  stimes n (Sum a) = Sum (fromIntegral n * a)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

(Sum 500) <> (Sum 400) <> (Sum 100)
==> Sum {getSum = 1000}

newtype Product a = Product { getProduct :: a } 
  deriving (Eq, Ord, ...)

instance Num a => Semigroup (Product a) where
  (<>) = coerce ((*) :: a -> a -> a)
  stimes n (Product a) = Product (a ^ n)

instance Num a => Monoid (Product a) where
  mempty = Product 1
```

## WHY?

Why even define algebraic structures?
Abstraction!
Monoid and Semigroup are implicit promises on semantics of our programs

Is abstraction helpful? Yes!

**Use case example**: 
Distributed computation of a huge term

```
... <> (a <> b) <> c <> (d <> (e <> f)) <> ...
```

We can create a framework that works on Semigroups and distributes to multiple machines.
We know they are associative so we could optimize the distribution

**Use case example**:
Mathematical properties make mathematical proofs possible

```hs
import Test.QuickCheck

prop x y ((x <> y) <> mempty) === (x <> (y <> mempty))
  where types = (x :: [Int], y :: [Int])
```
