# Chapter 13 - The Burrows–Wheeler transform

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
    lrot (x:xs) = xs ++ [x]
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

Then we can define `untransform` as: 

```haskell
untransform (ys, k) = (recreate ys) !! k
```

## Recreational Calculation

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

```haskell
rrot :: [a] -> [a]
rrot xs = [last xs] ++ init xs

hdsort :: Ord a => [[a]] -> [[a]]
hdsort = sortBy cmp
  where 
    cmp (x:xs) (y:ys) = compare x y

consCol :: ([a], [[a]]) -> [[a]]
consCol (xs, xss) = zipWith (:) xs xss

fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f, g) x = (f x, g x)
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

Code in chap13d.hs.

## Let's Take It For A Spin


