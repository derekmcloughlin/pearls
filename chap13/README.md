# Chapter 13 - The Burrows Wheeler Transform

## Defining the BWT

```haskell
import Data.List

transform :: Ord a => [a] -> ([a], Int)
transform xs = (map last xss, position xs xss)
  where 
    xss = sort (rots xs)

position xs xss = length (takeWhile (/= xs) xss)

rots :: [a] -> [[a]]
rots xs = take (length xs) (iterate lrot xs)
  where 
    lrot :: [a] -> [a]
    lrot [] = []
    lrot (y:ys) = ys ++ [y]
```

Code in chap13a.hs.

Running this we can see the rotations and transformations of the string "yokohama":

```haskell
ghci> :l chap13a.hs
ghci> rots "yokohama"
["yokohama","okohamay","kohamayo","ohamayok","hamayoko","amayokoh","mayokoha","ayokoham"]
ghci> transform "yokohama"
("hmooakya",7)
```

We can also see the function bringing together characters with a common context:

```haskell
ghci> transform "this, that or the other"
("te,rshhhtttth  oeia  or",22)
```

The goal of the rest of the chapter is to define the reverse transformation `untransform` such
that:

```haskell
untransform :: Ord a => ([a], Int) -> [a]

untransform . transform = id
```

We use a function `recreate` to help with the following properties:

```haskell
recreate :: Ord a => [a] -> [[a]] 

recreate . map last . sort . rots = sort . rots
```

So, given the last column of the sorted rotation matrix produced by `transform`, 
the `recreate` function can recreate the first j columns of the original sorted 
rotation matrix. 

Then we can define `untransform` as: 

```haskell
untransform (ys, k) = (recreate ys) !! k
```

## Recreational Calculation

The `consCol` function takes a list, and a list of lists, and prepends each element of the
first list onto each element of the second list.

```haskell
consCol :: ([a], [[a]]) -> [[a]]
consCol (xs, xss) = zipWith (:) xs xss
```

```haskell
ghci:> consCol ("ABCDEFG", ["hello", "world", "how", "are", "you"])
["Ahello","Bworld","Chow","Dare","Eyou"]
```

`hdsort` sorts a matrix based on its first column.

```haskell
hdsort :: Ord a => [[a]] -> [[a]]
hdsort = sortBy cmp
  where 
    cmp (x:xs) (y:ys) = compare x y
```

```haskell
ghci:> hdsort ["hello", "world", "how", "are", "you"]
["are","hello","how","world","you"]
```

The `takeCols` function takes the jth column of an n x n matrix, such as the
one produced by `rots` above:

```haskell
takeCols :: Int -> [[a]] -> [[a]]
takeCols j = map (take j)
```

We can see it in action:

```haskell
ghci> takeCols 3 $ rots "yokohama"
["yok","oko","koh","oha","ham","ama","may","ayo"]
ghci:> takeCols 3 [[1, 3, 5, 7, 9], [2, 4, 6, 9, 1]]
[[1,3,5],[2,4,6]]
```

The first case for `recreate` is straightforward:

```haskell
recreate :: Ord a => Int -> [a] -> [[a]]
recreate 0 = map (const [])
recreate _ = undefined
```

Applied to a list ys of length n, the result of `recreate 0` is a column of n
empty lists:

```haskell
ghci> recreate 0 "hmooakya"
["","","","","","","",""]
```

To get the more generic recursive solution, we define the following functions:

`fork` takes a pair of functions and a function argument and returns a pair
of results of those functions on that argument:

```haskell
fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f, g) x = (f x, g x)
```

```haskell
ghci:> fork (id, (+1)) 3
(3,4)
```

With these, the `recreate` function in full becomes:

```haskell
recreate :: Ord a => Int -> [a] -> [[a]]
recreate 0 = map (const [])
recreate j = hdsort . consCol . fork (id, recreate (j - 1))
```

And the `untransform` function is:

```haskell
-- Note: in the book, it doesn't mention that the first argument to the first 
-- call of `recreate` needs to be the length of the transformed string.
untransform :: Ord a => ([a], Int) -> [a]
untransform (ys, k) = (recreate (length ys) ys) !! k
```

> Note: in the book, it doesn't mention that the first argument to the first 
> call of `recreate` needs to be the length of the transformed string.

We can test this:

```haskell
ghci> let s = "Now is the time for all good men to come to the aid of their party."
ghci> let t = transform s
ghci> let u = untransform t
ghci> s
"Now is the time for all good men to come to the aid of their party."
ghci> t
"Now is the time for all good men to come to the aid of their party."
ghci> s == t
True
```

Code in chap13b.hs

## A Faster Algorithm

The search for a faster algorithm starts off with defining a function `apply`
such that:

```haskell
sort ys = apply p ys
```

This gives us (using `sort'` instead of `sort`, and taking out the locally-defined
versions of `p` and `apply`):

```haskell
sort' ys = apply q ys
  where
    q = p ys

p :: Ord a => [a] -> [Int]
p ys = map snd (sort (zip ys [0 .. n - 1]))
  where
    n = length ys

apply :: [Int] -> [a] -> [a]
apply p ys = [ys !! (p !! i ) | i <- [0 .. n - 1]]
  where
    n = length ys
```

Here, `p` gives us the indexes of the items in the list
as they would appear if the list was sorted.

```haskell
ghci> p "hello"
[1,0,2,3,4]
```

`apply` just takes those indexes and iterates through the list to give 
the sorted version:

```haskell
ghic> apply [1, 0, 2, 3, 4] "hello"
"ehllo"
```

Given this, we arrive at a second version of `recreate`, which we call `recreate'`, and
a corresponding version of `untransform'`:

```haskell
recreate' :: Ord a => Int -> [a] -> [[a]]
recreate' 0 ys = map (const []) ys
recreate' j ys = (consCol . fork (apply q, apply q . (recreate' (j - 1)))) ys
  where
    q = p ys

untransform' :: Ord a => ([a], Int) -> [a]
untransform' (ys, k) = (recreate' (length ys) ys) !! k
```

Code in chap13c.hs.

### Solving the Recursion of recreate

The next step is to define `recreate` so that:

```haskell
recreate j = tp . take j . tail . iterate (apply p)
```

> Note: There's no Haskell built-in function called `tp`. The function `transpose` from `Data.List` is used instead to
> transpose a matrix.

The `transpose` function takes a matrix and transposes it:

```haskell
ghci> let x = ["abcde", "fghij", "klmno", "pqrst", "uvwxy"]
ghch> transpose x
["afkpu","bglqv","chmrw","dinsx","ejoty"]
```

Note that the transpose of the matrix generated by `rots` above is the matrix itself:

```haskell
ghci> let x = rots "yokohama"
ghci> x
["yokohama","okohamay","kohamayo","ohamayok","hamayoko","amayokoh","mayokoha","ayokoham"]
ghci> transpose x
["yokohama","okohamay","kohamayo","ohamayok","hamayoko","amayokoh","mayokoha","ayokoham"]
ghci> x == transpose x
True
```

After a lot of derivation, we arrive at the following:

```haskell
recreate'' j ys = (transpose . (take j) . tail . iterate (apply q) ) ys
  where
    q = p ys

untransform'' :: Ord a => ([a], Int) -> [a]
untransform'' (ys, k) = (recreate'' (length ys) ys) !! k
```

### Using Haskell Arrays Instead Of Lists

The final version of `untransform` uses Haskell's Data.Array for
constant-time access to array elements, instead of the linear-time
access of lists:

```haskell
import Data.Array

untransform''' (ys, k) = take n (tail (map (ya!) (iterate (pa!) k)))
  where 
    n = length ys
    ya = listArray (0, n - 1) ys
    pa = listArray (0, n - 1) (map snd (sort (zip ys [0 .. ])))
```

Code in chap13d.hs.

## Transform Revisited

The `transform` function can also be optimised further:

```haskell

rrot :: [a] -> [a]
rrot xs = [last xs] ++ init xs

transform' xs = ([xa!(pa!i) | i <- [0 .. n - 1]], k)
  where 
    n = length xs
    tag xs = xs ++ ['\0']
    k = length (takeWhile (/= 0) ps)
    xa = listArray (0, n - 1) (rrot xs)
    pa = listArray (0, n - 1) ps
    ps = map snd (sort (zip (tails (tag xs))[0 .. n - 1]))
```

Note that because of the use of `'\0'`, the type of the function
is now restricted to character arrays:

```haskell
ghci> :t transform'
transform' :: [Char] -> ([Char], Int)
```

Code also in chap13d.hs.

## Let's Take It For A Spin

Let's compare the first and last implementations of `untransform` on some 
large strings read in from a file.

Files `test-1.txt` to `test-4.txt` contain roughly double the number of bytes as the 
previous file. (The data is from The Ballad of Reading Gaol by Oscar Wilde).

```
ghc -O2 -prof -auto-all chap13b.hs
./chap13b +RTS -p test-1.txt
```

The timings and memory usage are as follows:

```
total time  =        0.43 secs   (433 ticks @ 1000 us, 1 processor)
total alloc = 477,709,672 bytes  (excludes profiling overheads)

total time  =        1.95 secs   (1948 ticks @ 1000 us, 1 processor)
total alloc = 2,078,450,928 bytes  (excludes profiling overheads)

total time  =        9.05 secs   (9049 ticks @ 1000 us, 1 processor)
total alloc = 9,283,988,216 bytes  (excludes profiling overheads)

total time  =       42.70 secs   (42705 ticks @ 1000 us, 1 processor)
total alloc = 39,493,625,344 bytes  (excludes profiling overheads)
```

Interestingly, if you double the size of the input, you quadruple the 
time taken. Also, the memory usage is through the roof!

For the final version in chap13d.hs:

```
total time  =        0.04 secs   (45 ticks @ 1000 us, 1 processor)
total alloc =  34,328,760 bytes  (excludes profiling overheads)


total time  =        0.23 secs   (233 ticks @ 1000 us, 1 processor)
total alloc = 135,721,120 bytes  (excludes profiling overheads)

total time  =        0.90 secs   (896 ticks @ 1000 us, 1 processor)
total alloc = 558,073,984 bytes  (excludes profiling overheads)

total time  =        3.57 secs   (3566 ticks @ 1000 us, 1 processor)
total alloc = 2,205,051,504 bytes  (excludes profiling overheads)
```

Performance has improved a lot, as has the memory usage.

However, it still looks like O(n^2).

