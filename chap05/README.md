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
my_compare :: Ord a => Num a => a -> a -> Ordering
my_compare x y = compare x y
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

my_compare :: Ord a => Num a => a -> a -> Ordering
my_compare x y = compare x y

main = putStrLn $ show $ sortBy my_compare [x + y | x <- xs, y <- ys]
```

Code is in chap05a.hs.

Compling and running as follows:

```
ghc -O2 -prof -auto-all chap05a.hs
./chap05a +RTS -p
```

produces the chap05a.prof file. The line we're interested in is the number of times
`my_compare` is called:

```
COST CENTRE   MODULE                  no.     entries  %time %alloc   %time %alloc

   my_compare Main                     88          84    0.0    0.0     0.0    0.0
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
[(-19,(1,1)),(-29,(1,2)),(-39,(1,3)),(-49,(1,4)),(-59,(1,5)),(-17,(2,1)),(-27,(2,2)),(-37,(2,3)),(-47,(2,4)),(-57,(2,5)),(-15,(3,1)),(-25,(3,2)),(-35,(3,3)),(-45,(3,4)),(-55,(3,5)),(-13,(4,1)),(-23,(4,2)),(-33,(4,3)),(-43,(4,4)),(-53,(4,5)),(-11,(5,1)),(-21,(5,2)),(-31,(5,3)),(-41,(5,4)),(-51,(5,5))]
```

In tabular format this looks like:

|  |   1|   2|   3|   4|   5|
|--|----|----|----|----|----|
|1 | -19| -29| -39| -49| -59|
|2 | -17| -27| -37| -47| -57|
|3 | -15| -25| -35| -45| -55|
|4 | -13| -23| -33| -43| -53|
|5 | -11| -21| -31| -41| -51|



```haskell
my_compare :: Ord a => Num a => (a, (Integer, Integer)) -> (a, (Integer, Integer)) -> Ordering
my_compare x y = compare x y

sortsums :: Ord a => Num a => [a] -> [a] -> [a]
sortsums xs ys = map fst (sortsubs xs (map negate ys))

sortsubs :: Ord a => Num a => [a] -> [a] -> [(a, (Integer, Integer))]
sortsubs xs ys = sortBy my_compare (subs xs ys)

subs :: Num a => [a] -> [a] -> [(a, (Integer, Integer))]
subs xs ys = [ (x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]
```

Note we changed `my_compare` to cater for the labels.

Code is in chap05b.hs.

Running this with profiling on we see the number of times `my_compare` is called:

```
COST CENTRE   MODULE                  no.     entries  %time %alloc   %time %alloc
   my_compare Main                     91          84    0.0    0.0     0.0    0.0
```

The number of comparisons is the same as before, as expected.

```haskell
ghci> :l chap05a.hs
ghci> let a = sortsums xs ys
ghci> c
[21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69]
ghci> isSorted c
True
ghci>
```

Reducing the Number of Comparisons
----------------------------------

We construct a table

```haskell
table :: Ord a => Num a => [a] -> [a] -> [(Integer, Integer, Integer)]
table xs ys = map snd (map (tag 1) xxs `merge` map (tag 2) yys) 
              where xxs = sortsubs xs xs
                    yys = sortsubs ys ys 
```

