Chapter 1 - The smallest free number
====================================

A naïve array-based solution
----------------------------
Original code (page 1)

```haskell
minfree :: [Nat] → Nat
minfree xs = head ([0 .. ] \\ xs)

(\\) :: Eq a -> [a] -> [a] -> [a] 
us\\vs = filter(∉ vs) us
```

I don't like the use of mathematical symbols in code, so let's re-write as:

```haskell
minfree :: [Integer] -> Integer
minfree xs = head([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = notElem n a
```

Haskell's notElem function works the other way around from ∉, so I've
defined the function notElem' that flips the arguments.

For easier testing I've also changed the data type to use Integer instead of Nat. More on
this later.

Testing this in ghci, we see that:

```haskell
gchi> :l chap01a.hs
[1 of 1] Compiling Main             ( chap01a.hs, interpreted )
Ok, modules loaded: Main.
gchi> let a = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
gchi> minfree a
15
```

It works. The code is in chapt01a.hs.


Using Data.Array.accumArray
---------------------------

In Haskell, an Array is a data type with a min/max bounds and a subscript operation.

```haskell
ghci> import Data.Array
ghci> let squares =  array (1,10) [(i, i*i) | i <- [1..10]]
ghci> squares
array (1,10) [(1,1),(2,4),(3,9),(4,16),(5,25),(6,36),(7,49),(8,64),(9,81),(10,100)]
ghci> squares!7
49
ghci> 
```

The list of items in the array is an association list, i.e. a list of pairs where the
first item in the list is the 'index' or key and the second item is the 'value'.

The `accumArray` function is used when manipulating these association lists. 

The example in the Haskell docs uses accumArray to produce a histogram of array values:

```haskell
ghci> let xs = [1, 2, 3, 4, 1, 2, 3, 1, 2, 6]
ghci> let hist bnds xs =  accumArray (+) 0 bnds [(x, 1) | x <- xs, inRange bnds x]
ghci> hist (1, 6) xs
array (1,6) [(1,3),(2,3),(3,2),(4,1),(5,0),(6,1)]
```

The bounds argument to hist will only give those values within the bounds:

```haskell
ghci> hist (1, 5) xs
array (1,5) [(1,3),(2,3),(3,2),(4,1),(5,0)]
```

On page 3, we see that accumArray can be used to sort a list in linear time where 
all members are in a certain range. However, if we use this definition, we get an error:

```haskell
ghci> let countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1)) where n = length xs
ghci> countlist xs

<interactive>:26:11:
    Couldn't match expected type `Int' with actual type `Integer'
    Expected type: [Int]
      Actual type: [Integer]
    In the first argument of `countlist', namely `xs'
    In the expression: countlist xs
```

There are a few issues with this.

```haskell
ghci> :t xs
xs :: [Integer]
```

We can fix this by using Integers instead of Ints:

```haskell
countlist :: [Integer] -> Array Integer Integer

countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1)) 
    where n = toInteger (length xs)
```

This is in chap01c.hs

```haskell
ghci> :l chap01c.hs
Ok, modules loaded: Main.
ghci> let xs = [1, 2, 3, 4, 1, 2, 3, 1, 2, 6]
ghci> countlist xs
Loading package array-0.4.0.0 ... linking ... done.
array (0,10) [(0,0),(1,3),(2,3),(3,2),(4,1),(5,0),(6,1),(7,0),(8,0),(9,0),(10,0)]
```

Now the sort becomes:

```haskell
ghci> let sort xs = concat [replicate k x | (x,k) <- countlist xs]

<interactive>:10:48:
    Couldn't match expected type `[t0]'
                with actual type `Array Integer Integer'
    In the return type of a call of `countlist'
    In the expression: countlist xs
    In a stmt of a list comprehension: (x, k) <- countlist xs
```

The problem here lies with the use of arrays in list comprehensions. Using the squares definition before, we
can't just simply have:

```haskell
ghci> [(x, y) | (x, y) <- squares]
```

We have to use the `assocs` function to get the key/value pairs:


```haskell
ghci> [(x, y) | (x, y) <- assocs squares]
[(1,1),(2,4),(3,9),(4,16),(5,25),(6,36),(7,49),(8,64),(9,81),(10,100)]
```

Now the sort is:

```haskell
ghci> let sort xs = concat [replicate k x | (x,k) <- assocs (countlist xs)]

<interactive>:45:33:
    Couldn't match expected type `Int' with actual type `Integer'
    In the first argument of `replicate', namely `k'
    In the expression: replicate k x
    In the first argument of `concat', namely
      `[replicate k x | (x, k) <- assocs (countlist xs)]'
```

Oh.

Int vs Integer
--------------

A lot of our problems with the code is the use of Int and Integer. `Int` is the more restrictive type. It matches the 
machine definition of integers. `Integer` is the more general arbitary-precision type. Not all Integers can
be Ints, but all Ints can be Integers.

```haskell
ghci> minBound::Int
-9223372036854775808
ghci> maxBound::Int
9223372036854775807
ghci> minBound::Integer

<interactive>:56:1:
    No instance for (Bounded Integer)
      arising from a use of `minBound'
    Possible fix: add an instance declaration for (Bounded Integer)
    In the expression: minBound :: Integer
    In an equation for `it': it = minBound :: Integer
```

In GHCI, we have:

```haskell
ghci> let a = 3
ghci> :t a
a :: Integer
```

This can cause problems. For example, the `replicate` function:

```haskell
ghci> :t replicate 
replicate :: Int -> a -> [a]
ghci> replicate 3 'z'
"zzz"
ghci> replicate a 'z'

<interactive>:64:11:
    Couldn't match expected type `Int' with actual type `Integer'
    In the first argument of `replicate', namely `a'
    In the expression: replicate a 'z'
    In an equation for `it': it = replicate a 'z'
```

In our definition of `countlist` before, we replaced `Int` with `Integer`, but it has come back to bite us. Let's
look at another solution.

```haskell
ghci> :t xs
xs :: [Integer]
ghci> let xs = [1, 2, 3, 4, 1, 2, 3, 1, 2, 6]::[Int]
xs :: [Int]
```

This means we can go back to our original definition of countlist:

```haskell
ghci> let countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat (1::Int))) where n = length xs
ghci> countlist xs
ghci> let sort xs = concat [replicate k x | (x,k) <- assocs (countlist xs)]
ghci> sort xs
[1,1,1,2,2,2,3,3,4,6]
```

Whew! The code is in chap01d.hs

Back to the original problem.
-----------------------------

Instead of using an array of integers, we use an array of `Bool`s.

```haskell
ghci> let checklist xs = accumArray (∨) False (0, n ) (zip (filter (≤ n) xs) (repeat True)) where n = length xs
```

The mathematical symbol ∨ is the boolean 'or' function. However, Haskell's `or` function takes an array of booleans:

```haskell
ghci> :t or
or :: [Bool] -> Bool
```

We have to use the `(||)` function instead.

```haskell
ghci> let checklist xs = accumArray (||) False (0, n ) (zip (filter (<= n) xs) (repeat True)) 
        where n = length xs

ghci> let a = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]::[Int]
ghci> checklist a
array (0,20) [(0,True),(1,True),(2,True),(3,True),(4,True),(5,True),(6,True),
              (7,True),(8,True),(9,True),(10,True),(11,True),(12,True),(13,True),
              (14,True),(15,False),(16,False),(17,True),(18,False),(19,True),(20,False)]
```

The first False entry is what we want. We see visually that it's 15.

Finally we have:

```haskell
import Data.Array

minfree = search . checklist

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n ) (zip (filter (<= n) xs) (repeat True)) 
    where n = length xs

search :: Array Int Bool -> Int 
search = length . takeWhile id . elems
```

The code is in chap01e.hs

NOTE: I couldn't get to define the `search` function in ghci, as it rejected the definiton:

```haskell
ghci> let search = length . takeWhile id . elems

<interactive>:5:38:
    Ambiguous type variable `i0' in the constraint:
      (Ix i0) arising from a use of `elems'
    Probable fix: add a type signature that fixes these type variable(s)
    In the second argument of `(.)', namely `elems'
    In the second argument of `(.)', namely `takeWhile id . elems'
    In the expression: length . takeWhile id . elems
```

Another Approach
----------------

Another way to write checklist is:

```haskell
checklist xs = runSTArray (do
        {a <- newArray (0, n) False;
        sequence [writeArray a x True | x <- xs, x <= n];
        return a}
    )
    where n = length xs

where n = length xs
```

This code is in chap01f.hs

```haskell
ghci> :l chap01f.hs [1 of 1] Compiling Main             ( chap01f.hs, interpreted )
Ok, modules loaded: Main.
ghci> let a = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]::[Int]
ghci> minfree a
15
```

A Divide & Conquer Solution
---------------------------

A more generic version of `minfree` is `minfrom`:

```haskell
minfree :: [Integer] -> Integer
minfree xs = minfrom 0 xs

minfrom :: Integer -> [Integer] -> Integer
minfrom a xs = head ([a ..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = notElem n a
```

The code is in chap01g.hs.

```haskell
ghci> :l chap01g.hs 
ghci> let a = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
ghci> minfree a
15
ghci> minfrom 3 a
15
ghci> minfrom 17 a
18
```


The heart of the divide and conquer solution is the use of the `partition` function in Haskell:

```haskell
ghci> import Data.List
ghci> let a = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
ghci> partition (<= 10) a
([8,9,0,1,10,7,4,5,3,2,6],[23,12,11,13,41,14,21,17,19])
```

With this, we have the divide and conquer solution:

```haskell
import Data.List

minfree :: [Integer] -> Integer

minfree xs = minfrom 0 (toInteger (length xs), xs)

minfrom :: Integer -> (Integer, [Integer]) -> Integer

minfrom a (n, xs)   | n == 0        = a
                    | m == b - a    = minfrom b (n - m, vs)
                    | otherwise     = minfrom a (m, us)
                      where (us, vs)    = partition (< b) xs
                            b           = a + 1 + n `div` 2
                            m           = toInteger (length us)
```

The code for this can be found in chapt01h.hs. I've gone back to using Integers again.

```haskell
ghci> :l chap01h.hs
ghci> let a = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
ghci> minfree a
15
```

References
----------

For more information on Haskell's arrays and association lists
see [The Gentle Introduction to Haskell](http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/arrays.html)
and [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/modules#data-map).


