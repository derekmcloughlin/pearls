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

Debugging the Problem
---------------------

The solutions in chap04b.hs and chap04c.hs are meant to be the same logically - they both divide and conquer
the problem in the same way. Let's add some Debug.Trace information into the two versions to see if that is the case.


The list based version with trace information:

```haskell
import Debug.Trace
import Text.Printf

smallest k ([], ws) = trace ("[], " ++ show ws) ws!!k
smallest k (zs, []) = trace (show zs ++ ", []") zs!!k
smallest k (zs, ws) =
    case (a < b, k <= p + q) of
        (True, True)   -> trace ("TT" ++ dump_vars k zs ws p q a b us vs) smallest k (zs, us)
        (True, False)  -> trace ("TF" ++ dump_vars k zs ws p q a b us vs) smallest (k - p - 1) (ys, ws)
        (False, True)  -> trace ("FT" ++ dump_vars k zs ws p q a b us vs) smallest k (xs, ws)
        (False, False) -> trace ("FF" ++ dump_vars k zs ws p q a b us vs) smallest (k - q - 1) (zs, vs)
    where p = (length zs) `div` 2
          q = (length ws) `div` 2
          (xs, a : ys) = splitAt p zs
          (us, b : vs) = splitAt q ws

dump_vars k zs ws p q a b us vs =
    " k = " ++ (printf "%2d" k :: String) ++ " " ++
    "zs = " ++ (printf "%15s" (show zs) :: String) ++ " " ++
    "ws = " ++ (printf "%15s" (show ws) :: String) ++ " " ++
    " p = " ++ (printf "%2d" p :: String) ++ " " ++
    " q = " ++ (printf "%2d" q :: String) ++ " " ++
    " a = " ++ (printf "%2d" a :: String) ++ " " ++
    " b = " ++ (printf "%2d" b :: String) ++ " " ++
    "us = " ++ (printf "%7s" (show us) :: String) ++ " " ++
    "vs = " ++ (printf "%7s" (show vs) :: String) ++ " " ++
    "kd = " ++ (printf "%2d" (p + q) :: String) 

xs = [1, 2, 3, 9]
ys = [4, 10, 13, 33, 67]
```

Code is in chap04d.hs.

The array-based version with trace information:


```haskell
import Data.Array
import Debug.Trace

smallest :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest k (xa, ya) = search k (0, m + 1) (0, n + 1)
    where (0, m) = bounds xa
          (0, n) = bounds ya
          search k (lx, rx) (ly,  ry)
            | lx == rx  = trace ("lx == rx" ++ dump_vars k lx rx ly ry mx my) ya!k
            | ly == ry  = trace ("ly == ry" ++ dump_vars k lx rx ly ry mx my) xa!k
            | otherwise = case (xa!mx < ya!my, k <= mx + my) of
                    (True, True)   -> trace ("TT: " ++ dump_vars k lx rx ly ry mx my) search k (lx, rx) (ly, my)
                    (True, False)  -> trace ("TF: " ++ dump_vars k lx rx ly ry mx my) search (k - mx - 1) (mx, rx) (ly, ry)
                    (False, True)  -> trace ("FT: " ++ dump_vars k lx rx ly ry mx my) search k (lx, mx) (ly, ry)
                    (False, False) -> trace ("FF: " ++ dump_vars k lx rx ly ry mx my) search (k - my - 1) (lx, rx) (my, ry)
            where mx = (lx + rx) `div` 2
                  my = (ly + ry) `div` 2

dump_vars k lx rx ly ry mx my =
    " k = " ++ show k ++ "\t" ++
    "lx = " ++ show lx ++ "\t" ++
    "rx = " ++ show rx ++ "\t" ++
    "ly = " ++ show ly ++ "\t" ++
    "ry = " ++ show ry ++ "\t" ++
    "a = " ++ (if (0, mx) <= bounds xa then show(xa!mx) else "-") ++ "\t" ++
    "b = " ++ (if (0, my) <= bounds ya then show(ya!my) else "-") ++ "\t" ++
    "mx = " ++ show mx ++ "\t" ++
    "my = " ++ show my ++ "\t" ++
    "kd = " ++ show (mx + my - lx - ly)

xs = [1, 2, 3, 9]
ys = [4, 10, 13, 33, 67]

xa = listArray(0, length xs - 1) xs
ya = listArray(0, length ys - 1) ys
```

Running the list based version to find the 3rd smallest number:

```
ghci> smallest 3 (xs, ys)
TT k =  3 zs =       [1,2,3,9] ws = [4,10,13,33,67]  p =  2  q =  2  a =  3  b = 13 us =  [4,10] vs = [33,67] kd =  4
TT k =  3 zs =       [1,2,3,9] ws =          [4,10]  p =  2  q =  1  a =  3  b = 10 us =     [4] vs =      [] kd =  3
TF k =  3 zs =       [1,2,3,9] ws =             [4]  p =  2  q =  0  a =  3  b =  4 us =      [] vs =      [] kd =  2
FT k =  0 zs =             [9] ws =             [4]  p =  0  q =  0  a =  9  b =  4 us =      [] vs =      [] kd =  0
[], [4]
```

The same one on the array-based version:

```
ghci> smallest 3 (xa, ya)
TT:  k = 3      lx = 0  rx = 4  ly = 0  ry = 5  a = 3   b = 13  mx = 2  my = 2  kd = 4
TT:  k = 3      lx = 0  rx = 4  ly = 0  ry = 2  a = 3   b = 10  mx = 2  my = 1  kd = 3
TF:  k = 3      lx = 0  rx = 4  ly = 0  ry = 1  a = 3   b = 4   mx = 2  my = 0  kd = 2
FT:  k = 0      lx = 2  rx = 4  ly = 0  ry = 1  a = 9   b = 4   mx = 3  my = 0  kd = 1
TT:  k = 0      lx = 2  rx = 3  ly = 0  ry = 1  a = 3   b = 4   mx = 2  my = 0  kd = 0
ly == ry k = 0  lx = 2  rx = 3  ly = 0  ry = 0  a = 3   b = 4   mx = 2  my = 0  kd = 0
1
```

We can immediately see that the two solutions don't follow the same path.

The problem here is in the way the indexes work in the 2nd solution. The first solution breaks down the list but the indexes p and q are
always zero-based. However, the second solution doesn't do anything with the array but uses indexes that are relative to the `lx` and `ly` variables.

The solution fixes this problem is in chap04f.hs:

```haskell
import Data.Array
import Debug.Trace

smallest :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest k (xa, ya) = search k (0, m + 1) (0, n + 1)
    where (0, m) = bounds xa
          (0, n) = bounds ya
          search k (lx, rx) (ly,  ry)
          --                              changes here:                           vvvvvv
            | lx == rx  = trace ("lx == rx" ++ dump_vars k lx rx ly ry mx my) ya!(ly + k)
          --                              changes here:                           vvvvvv
            | ly == ry  = trace ("ly == ry" ++ dump_vars k lx rx ly ry mx my) xa!(lx + k)
          --                              changes here:     vvvvvvvvv
            | otherwise = case (xa!mx < ya!my, k <= mx + my - lx - ly) of
                    (True, True)   -> trace ("TT: " ++ dump_vars k lx rx ly ry mx my) search k (lx, rx) (ly, my)
          --                              changes here:                                                  vvvv   vvvvvv
                    (True, False)  -> trace ("TF: " ++ dump_vars k lx rx ly ry mx my) search (k - mx - 1 + lx) (mx + 1, rx) (ly, ry)
                    (False, True)  -> trace ("FT: " ++ dump_vars k lx rx ly ry mx my) search k (lx, mx) (ly, ry)
          --                              changes here:                                                  vvvv   vvvvvv
                    (False, False) -> trace ("FF: " ++ dump_vars k lx rx ly ry mx my) search (k - my - 1 + ly) (lx, rx) (my + 1, ry)
            where mx = (lx + rx) `div` 2
                  my = (ly + ry) `div` 2


dump_vars k lx rx ly ry mx my =
    " k = " ++ show k ++ "\t" ++
    "lx = " ++ show lx ++ "\t" ++
    "rx = " ++ show rx ++ "\t" ++
    "ly = " ++ show ly ++ "\t" ++
    "ry = " ++ show ry ++ "\t" ++
    "a = " ++ (if (0, mx) <= bounds xa then show(xa!mx) else "-") ++ "\t" ++
    "b = " ++ (if (0, my) <= bounds ya then show(ya!my) else "-") ++ "\t" ++
    "mx = " ++ show mx ++ "\t" ++
    "my = " ++ show my ++ "\t" ++
    "kd = " ++ show (mx + my - lx - ly)

xs = [1, 2, 3, 9]
ys = [4, 10, 13, 33, 67]

xa = listArray(0, length xs - 1) xs
ya = listArray(0, length ys - 1) ys
```

Running this gives:

```
ghci> smallest 3 (xa, ya)
Loading package array-0.4.0.1 ... linking ... done.
TT:  k = 3      lx = 0  rx = 4  ly = 0  ry = 5  a = 3   b = 13  mx = 2  my = 2  kd = 4
TT:  k = 3      lx = 0  rx = 4  ly = 0  ry = 2  a = 3   b = 10  mx = 2  my = 1  kd = 3
TF:  k = 3      lx = 0  rx = 4  ly = 0  ry = 1  a = 3   b = 4   mx = 2  my = 0  kd = 2
FT:  k = 0      lx = 3  rx = 4  ly = 0  ry = 1  a = 9   b = 4   mx = 3  my = 0  kd = 0
lx == rx k = 0  lx = 3  rx = 3  ly = 0  ry = 1  a = 9   b = 4   mx = 3  my = 0  kd = 0
4
```

The traceless version is in chap04g.hs:


```haskell
import Data.Array

smallest :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest k (xa, ya) = search k (0, m + 1) (0, n + 1)
    where (0, m) = bounds xa
          (0, n) = bounds ya
          search k (lx, rx) (ly,  ry)
            | lx == rx  = ya!(ly + k)
            | ly == ry  = xa!(lx + k)
            | otherwise = case (xa!mx < ya!my, k <= mx + my - lx - ly) of
                    (True, True)   -> search k (lx, rx) (ly, my)
                    (True, False)  -> search (k - mx - 1 + lx) (mx + 1, rx) (ly, ry)
                    (False, True)  -> search k (lx, mx) (ly, ry)
                    (False, False) -> search (k - my - 1 + ly) (lx, rx) (my + 1, ry)
            where mx = (lx + rx) `div` 2
                  my = (ly + ry) `div` 2
```


