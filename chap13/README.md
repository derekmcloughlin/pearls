Chapter 13 - The Burrows–Wheeler transform
=============================

Defining the BWT
-------------


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

Recreational Calculation
------------------------

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


