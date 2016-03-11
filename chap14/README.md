# Chapter 14 - The Last Tail

The tails of a list are given by the `tails` function of `Data.List`.

```haskell
ghci> import Data.List
ghci> tails "hello"
["hello","ello","llo","lo","o",""]
```

We're looking for an O(n) solution for `maxtail`, the maximum of the tails of a list.

We start off with the inductive definition:

```haskell
maxtail :: Ord a => [a] -> [a]
maxtail = foldl op []

op :: Ord a => [a] -> a -> [a]
op ys x = maximum [zs ++ [x ] | zs <- tails ys]
```

Code in chap14a.hs.

This works, but is not O(n):

```haskell
ghci> maxtail "tomato"
"tomato"
ghci> maxtail "introduction"
"uction"
```

## Borders

As part of finding a new version of `op`, the function `borders` is 
introduced. A list `xs` is a border of `ys` if `xs` is both a suffix and 
a prefix of `ys`.

We can define this as follows:

```haskell
borders xs = [ys | ys <- tails xs, ys `isPrefixOf` xs]
```

```haskell
ghci> borders "7412741274"
["7412741274","741274","74",""]
ghci> borders "11111111"
["11111111","1111111","111111","11111","1111","111","11","1",""]
ghci> borders "mammam"
["mammam","mam","m",""]
```

With this definition we can re-define `op` as:

```haskell
op :: Ord a => [a] -> a -> [a]
op ys x = maximum [zs ++ [x] | zs <- borders ys]
```

We can test `maxtail` as before:

```haskell
ghci> maxtail "tomato"
"tomato"
ghci> maxtail "introduction"
"uction"
```

Code in chap14b.hs.

### Border

We would like to define `borders` in terms of 
the function `border`, which gives the maximum border of a list, as follows:

```haskell
borders :: Eq a => [a] -> [[a]]
borders [] = [[]]
borders xs = xs : borders (border xs)
```

We start by defining the function `after` such that:

```haskell
(xs ++ ys) `after` xs = ys
```

> Note: Rather than use the Unicode down-arrow symbol directly in Haskell code, I've
> defined the function 'after' instead.

An implementation of this is as follows:

```haskell
after :: Eq a => [a] -> [a] -> [a]
after [] ys = ys
after xs [] = xs
after (x:xs) (y:ys) 
    | x == y = after xs ys
    | otherwise = (x:xs)
```

We can use this in an infix form:

```haskell
ghci> "helloworld" `after` "hello"
"world"
```

In a simlar way, we can define a `before` function such that:

```haskell
us = (us `before` vs) ++ vs
```

A simple implementation is just to re-use `after` on reversed lists:

```haskell
before :: Eq a => [a] -> [a] -> [a]
before xs ys = reverse $ after (reverse xs) (reverse ys)
```

Trying it out:

```haskell
ghci> "helloworld" `before` "world"
"hello"
```

> Note: the `before` function won't work properly if the second argument
> isn't a tail of the first argument.

With this in place, we define the function `border` as follows:

```haskell
border :: Eq a => [a] -> [a]
border xs
    | xs == []                  = []
    | length(xs) == 1           = []
    | head (ys `after` zs) == x = zs ++ [x]
    | otherwise                 = border (zs ++ [x])
  where 
    ys = init xs 
    x = last xs 
    zs = border ys
```

Trying it out we have:

```haskell
ghci> borders "7412741274"
["7412741274","741274","74",""]
ghci> borders "11111111"
["11111111","1111111","111111","11111","1111","111","11","1",""]
ghci> borders "mammam"
["mammam","mam","m",""]
```

And `maxtail` again:

```haskell
ghci> maxtail "tomato"
"tomato"
ghci> maxtail "introduction"
"uction"
```

Code in chap14c.hs

### Modified Version of "border"

The book shows an "optimised" verison of `border` on page 106:

```haskell
border :: Ord a => [a] -> [a]
border xs 
    | xs == []                   = []
    | length(xs) == 1            = []
    | ys_after_zs == []          = []
    | head(ys_after_zs) < x      = border (zs ++ [x])
    | head(ys_after_zs) == x     = (zs ++ [x])
    | head(ys_after_zs) > x      = []   -- Problem with this line?
  where 
    ys = init xs 
    x = last xs 
    zs = border ys 
    ys_after_zs = (ys `after` zs) 
```

However, I couldn't get it to work on the following input:

```haskell
ghci> border "mammam"
["mammam","mam","m",""] -- correct
ghci> borders "bobbob"
["bobbob","b",""]       -- incorrect
```

The problem (I think) lies with the line:

```haskell
    | head(ys_after_zs) > x      = []
```

, which probably should be:


```haskell
    | head(ys_after_zs) > x      = border (zs ++ [x])
```

Which means the definition of `border` is:

```haskell
border :: Ord a => [a] -> [a]
border xs 
    | xs == []                   = []
    | length(xs) == 1            = []
    | ys_after_zs == []          = []
    | head(ys_after_zs) < x      = border (zs ++ [x])
    | head(ys_after_zs) == x     = (zs ++ [x])
    | head(ys_after_zs) > x      = border (zs ++ [x])
  where 
    ys = init xs 
    x = last xs 
    zs = border ys 
    ys_after_zs = (ys `after` zs) 
```

However, this is exactly the same as the version in chap14c.hs. Am I missing something here?

Code in chap14d.hs if you want to check.

## Cocktail

We define versions of `maxtail` and `op` that uses `cocktail`:

```haskell
maxtail' :: Ord a => [a] -> [a]
maxtail' = uncurry (++) . cocktail'

cocktail' :: Ord a => [a] -> ([a], [a])
cocktail' = foldl op' ([], [])

op' :: Ord a => ([a], [a]) -> a -> ([a], [a])
op' (zs, ws) x 
    | null ws   = ([], [x])
    | w < x     = cocktail' (zs ++ [x])
    | w == x    = (zs ++ [x], tail ws ++ [x])
    | w > x     = ([], zs ++ ws ++ [x])
  where 
    w = head ws

```

> Note we call the function cocktail' to keep the naming consistent.

Code in chap14e.hs.

Note that the types of the functions has changed.

## Reducing The Problem Size

```haskell
maxtail'' :: Ord a => [a] -> [a]
maxtail'' = uncurry (++) . cocktail''

cocktail'' :: Ord a => [a] -> ([a], [a])
cocktail'' = foldl op'' ([], [])

op'' (zs, ws) x 
    | null ws   = ([], [x])
    | w < x     = cocktail'' (take r zs ++ [x])
    | w == x    = (zs ++ [x], tail ws ++ [x])
    | w > x     = ([], zs ++ ws ++ [x])
  where 
    w = head ws
    r = (length zs) `mod` (length ws)
```

Code in chap14f.hs

## Final Optimisations

```haskell
maxtail''' :: Ord a => [a] -> [a]
maxtail''' [] = []
maxtail''' (x:xs) = step (0, 1, x:xs, x:xs, xs)

step (p, q, ys, ws, []) = ys
step (p, q, ys, w : ws, x:xs)
    | w < x     = maxtail''' (drop (q - r) (w:ws))
    | w == x    = step (p + 1, q, ys, ws, xs)
    | w > x     = step (0, p + q + 1, ys, ys, xs)
    where r = p `mod` q
```

Code in chap14g.hs.

## Testing Performance


