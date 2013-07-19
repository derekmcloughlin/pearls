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

Divide and Conquer
------------------

```haskell
smallest k ([], ws) = ws !! k
smallest k (zs, []) = zs !! k
smallest k (zs, ws) =
    case (a < b, k <= p + q) of
        (True, True)   -> smallest k (zs, us)
        (True, False)  -> smallest (k - p - 1) (ys, ws)
        (False, True)  -> smallest k (xs, ws)
        (False, False) -> smallest (k - q - 1) (zs, vs)
    where p = (length zs) `div` 2
          q = (length ws) `div` 2
          (xs, a : ys) = splitAt p zs
          (us, b : vs) = splitAt q ws
```

Running:

```haskell
ghci> smallest 3 ([1, 2, 3, 9], [4, 10, 13, 33, 67])
4
```


Code in chap04b.hs

Using Arrays instead of Lists
-----------------------------

```haskell
import Data.Array

smallest :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest k (xa, ya) = search k (0, m + 1) (0, n + 1)
    where (0, m) = bounds xa
          (0, n) = bounds ya
          search k (lx, rx) (ly,  ry)
            | lx == rx  = ya!k
            | ly == ry  = xa!k
            | otherwise = case (xa!mx < ya!my, k <= mx + my) of
                    (True, True)   -> search k (lx , rx) (ly, my)
                    (True, False)  -> search (k - mx - 1) (mx, rx) (ly, ry)
                    (False, True)  -> search k (lx , mx) (ly, ry)
                    (False, False) -> search (k - my - 1) (lx, rx) (my, ry)
            where mx = (lx + rx) `div` 2
                  my = (ly + ry) `div` 2

```

Running:

```haskell
ghci> let xs = [1, 2, 3, 9]
ghci> let ys = [4, 10, 13, 33, 67]
ghci> let xa = listArray(0, length xs - 1) xs
ghci> let ya = listArray(0, length ys - 1) ys
ghci> smallest 3 (xa, ya)
1
```

Oops. Something is not right.


