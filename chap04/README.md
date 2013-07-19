Chapter 4 - A Selection Problem
===============================

First Steps
-----------

```haskell
smallest :: Ord a => Int -> ([a], [a]) -> a
smallest k (xs, ys) = union (xs, ys) !! k

union (xs, [ ]) = xs
union ([ ], ys) = ys
union (x : xs, y : ys) 
    | x < y     = x : union (xs, y : ys)
    | x > y     = y : union (x : xs, ys)
```

Note that when using these functions the lists *must* already be ordered.

```haskell
ghci> union ([1, 2, 3, 9], [4, 10, 13, 33, 67])
[1,2,3,4,9,10,13,33,67]
ghci> smallest 3 ([1, 2, 3, 9], [4, 10, 13, 33, 67])
4
```

Also note that `smallest 3` is actually looking for the 4th smallest (zero-based).

Code in chap04a.hs.


