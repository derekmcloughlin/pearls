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

## Borders and Border

As part of finding a new version of `op`, the function `borders` is introduced:

```haskell
borders [ ] = [[ ]]
borders xs = xs : borders (border xs)

border (ys ++ [x]) 
    | head (ys ? zs) < x    = border (zs ++ [x])
    | head (ys ? zs) x      = zs ++ [x]
    | head (ys ? zs) > x    = []
  where 
    zs = border ys
```

The operator `|v` is defined such that:

```haskell
(xs ++ ys) |v xs = ys
```

An implementation of this is as follows:


