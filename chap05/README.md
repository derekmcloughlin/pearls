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

First Attempt
-------------

```haskell
sortsums :: Ord a => Num a => [a] -> [a] -> [a]
sortsums xs ys = map fst (sortsubs xs (map negate ys))

sortsubs :: Ord a => Num a => [a] -> [a] -> [(a, (Integer, Integer))]
sortsubs xs ys = sort (subs xs ys)

subs :: Num a => [a] -> [a] -> [(a, (Integer, Integer))]
subs xs ys = [ (x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]
```

Code is in chap05a.hs.

```haskell
ghci> :l chap05a.hs
ghci> let a = sortsums xs ys
ghci> c
[21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69]
ghci> isSorted c
True
ghci>
```


