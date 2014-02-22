Chapter 5 - Sorting Pairwise Sums
===============================

Lambert's Algorithm
-------------------

For the purposes of this chapter we need a monotonic function ⊕ and it's 
negation ⊖ . Let's use ordinary addition and subtraction for this.

The following properties are satisfied:

    x <= x', y <= y' => x + y <= x' + y'
    x + y = x - negate y
    x - y <= x' - y' => x - x' <= y - y'

where negate can be the standard Haskell `negate` function:

```haskell
ghci> :t negate
negate :: Num a => a -> a
ghci> negate 3
-3
ghci> negate (-1)
1
```

For testing:

```haskell
let xs = [7, 3, 1, 9, 5]
let ys = [40, 20, 30, 60, 50]
```

To test if a list is sorted we could hand-roll our own function:

```haskell
isSorted xs = (sort xs) == xs
```

Alternatively we can use the `isSorted` function from Data.List.Ordered.

We're interested in the number of comparisons being made during the sort. To see
this clearly let's define a comparison function:

```haskell
cmp :: Ord a => Num a => a -> a -> Ordering
cmp x y = compare x y
```

We can add profiling when compiling the program to tell us the number of times
the comparison function was called.

The Naive Attempt
-----------------

Before diving into Bird's solution, a very naive solution might be as follows:


```haskell
import Data.List

xs = [1, 3, 5, 7, 9]
ys = [20, 30, 40, 50, 60]

cmp :: Ord a => Num a => a -> a -> Ordering
cmp x y = compare x y

main = putStrLn $ show $ sortBy cmp [x + y | x <- xs, y <- ys]
```

Code is in chap05a.hs.

Compling and running as follows:

```
ghc -O2 -prof -auto-all chap05a.hs
./chap05a +RTS -p
```

produces the chap05a.prof file. The line we're interested in is the number of times
`cmp` is called:

```
COST CENTRE   MODULE                  no.     entries  %time %alloc   %time %alloc

          cmp Main                     88         105    0.0    0.0     0.0    0.0
```
 
First Attempt
-------------

The table of labelled subtractions

```haskell
subs :: Num a => [a] -> [a] -> [(a, (Integer, Integer))]
subs xs ys = [ (x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]
```

gives us the following:

```haskell
ghci> subs xs ys
[(-19,(1,1)),(-29,(1,2)),(-39,(1,3)),(-49,(1,4)),(-59,(1,5)),
 (-17,(2,1)),(-27,(2,2)),(-37,(2,3)),(-47,(2,4)),(-57,(2,5)),
 (-15,(3,1)),(-25,(3,2)),(-35,(3,3)),(-45,(3,4)),(-55,(3,5)),
 (-13,(4,1)),(-23,(4,2)),(-33,(4,3)),(-43,(4,4)),(-53,(4,5)),
 (-11,(5,1)),(-21,(5,2)),(-31,(5,3)),(-41,(5,4)),(-51,(5,5))]
```

In tabular format this looks like:

i/j | 1   | 2   | 3   | 4   | 5  
--- | --- | --- | --- | --- | ---
1   | -19 | -29 | -39 | -49 | -59
2   | -17 | -27 | -37 | -47 | -57
3   | -15 | -25 | -35 | -45 | -55
4   | -13 | -23 | -33 | -43 | -53
5   | -11 | -21 | -31 | -41 | -51

We define `sortsubs` to sort this:

```haskell
cmp :: Ord a => Num a => (a, (Int, Int)) -> (a, (Int, Int)) -> Ordering
cmp x y = compare x y

sortsubs :: Ord a => Num a => [a] -> [a] -> [(a, (Int, Int))]
sortsubs xs ys = sortBy cmp (subs xs ys)

subs :: Num a => [a] -> [a] -> [(a, (Int, Int))]
subs xs ys = [ (x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]
```

Note we changed `cmp` to cater for the labels.


Finally we define `sortsums` as follows:

```haskell
sortsums :: Ord a => Num a => [a] -> [a] -> [a]
sortsums xs ys = map fst (sortsubs xs (map negate ys))
```

Here's what it looks like:

```haskell
ghci> map negate ys
[-20,-30,-40,-50,-60]

ghci> subs xs (map negate ys)
[(21,(1,1)),(31,(1,2)),(41,(1,3)),(51,(1,4)),(61,(1,5)),
 (23,(2,1)),(33,(2,2)),(43,(2,3)),(53,(2,4)),(63,(2,5)),
 (25,(3,1)),(35,(3,2)),(45,(3,3)),(55,(3,4)),(65,(3,5)),
 (27,(4,1)),(37,(4,2)),(47,(4,3)),(57,(4,4)),(67,(4,5)),
 (29,(5,1)),(39,(5,2)),(49,(5,3)),(59,(5,4)),(69,(5,5))]

ghci> sortsubs xs (map negate ys)
[(21,(1,1)),(23,(2,1)),(25,(3,1)),(27,(4,1)),(29,(5,1)),
 (31,(1,2)),(33,(2,2)),(35,(3,2)),(37,(4,2)),(39,(5,2)),
 (41,(1,3)),(43,(2,3)),(45,(3,3)),(47,(4,3)),(49,(5,3)),
 (51,(1,4)),(53,(2,4)),(55,(3,4)),(57,(4,4)),(59,(5,4)),
 (61,(1,5)),(63,(2,5)),(65,(3,5)),(67,(4,5)),(69,(5,5))]

ghci> map fst (sortsubs xs (map negate ys))
[21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69]
```

Code is in chap05b.hs.

Running this with profiling on we see the number of times `cmp` is called:

```
COST CENTRE   MODULE                  no.     entries  %time %alloc   %time %alloc
          cmp Main                     91          84    0.0    0.0     0.0    0.0
```

The number of comparisons is the same as before, as expected.

```haskell
ghci> :l chap05b.hs
ghci> let a = sortsums xs ys
ghci> c
[21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69]
ghci> isSorted c
True
ghci>
```

Reducing the Number of Comparisons
----------------------------------

We construct a table that has tagged values:

```haskell
table :: Ord a => Num a => [a] -> [a] -> [(Int, Int, Int)]
table xs ys = map snd (map (tag 1) xxs `merge` map (tag 2) yys) 
              where xxs = sortsubs' xs
                    yys = sortsubs' ys

tag :: Int -> (a, (Int, Int)) -> (a, (Int, Int, Int))
tag i (x,(j,k)) = (x,(i,j,k))
```

This is used in the `mkArray` function:

```haskell
mkArray :: Ord a => Num a => [a] -> [a] -> Array (Int, Int, Int) Integer
mkArray xs ys = array b (zip (table xs ys) [1..])
                where b = ((1, 1, 1), (2, p, p))
                      p = max (length xs) (length ys)
```

The implementation of `sortsubs'` is:

```haskell
sortsubs' :: Ord a => Num a => [a] -> [(a, (Int, Int))]
sortsubs' []    = []
sortsubs' [w]   = [(w - w, (1, 1))]
sortsubs' ws = foldr1 (merge) [xxs, map (incr m) xys, map (incl m) yxs, map (incb m) yys]
    where xxs               = sortsubs' xs
          xys               = sortBy (cmp (mkArray xs ys )) (subs xs ys ) 
          yxs               = map switch (reverse xys )
          yys               = sortsubs' ys
          (xs, ys)          = splitAt m ws
          m                 = (length ws) `div` 2
          incl m (x,(i,j))  = (x, (m + i, j)) 
          incr m (x,(i,j))  = (x, (i, m + j))
          incb m (x,(i,j))  = (x, (m + i, m + j))
          switch (x,(i,j))  = (negate x, (j, i))
```

The `cmp` function needs to be changed:

```haskell
cmp a (_, (i, j)) (_, (k, l)) = compare (a ! (1, i, k)) (a ! (2, j, l))
```

Note that the comparison doesn't actually use the x or y values.

The final code is in chap05c.hs.

```haskell
ghci> :l chap05c.hs
ghci> sortsums xs ys
[21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69]
```

If we look at the profiling information for `cmp` we see:

```
ghc -O2 -prof -auto-all chap05c.hs
./chap05c +RTS -p
grep cmp chap05c.prof
     cmp                   Main                    134         105    0.0    0.0     0.0    0.0
          cmp              Main                    133          12    0.0    0.0     0.0    0.0
          cmp              Main                    119          17    0.0    0.0     0.0    0.0
```

The results aren't good. The number of calls to `cmp` has increased - the original 105 + 29.

Needs further investigation.




