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

As part of finding a new version of `op`, the function `borders` is introduced. 
However, we need to define a couple of helper functions first:

The operator `after` is defined such that:

```haskell
(xs ++ ys) `after` xs = ys
```

(Rather than use the Unicode symbol directly in Haskell code, I've
defined the function 'after').

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

With this in place, we define the functino `borders` as follows:

```haskell
borders :: Ord a => [a] => [[a]]
borders [] = [[]]
borders xs = xs : borders (border xs)

border (ys ++ [x]) 
    | head (ys ? zs) < x    = border (zs ++ [x])
    | head (ys ? zs) x      = zs ++ [x]
    | head (ys ? zs) > x    = []
  where 
    zs = border ys
```


