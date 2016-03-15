# Chapter 15 - All The Common Prefixes

## Introduction

We're looking for the function `allcp` defined in terms of `llcp`:

```haskell
allcp Eq a => [a] -> [Int]
allcp xs = map (llcp xs) (tails xs)
```

Taking xs = "abacabacab", this should give:

```haskell
ghci> let xs = "abacabacab"
ghci> tails xs
["abacabacab","bacabacab","acabacab","cabacab","abacab","bacab","acab","cab","ab","b",""]
ghci> map (llcp xs) (tails xs)
works out as:
        llcp "abacabacab" "abacabacab"      => 10
        llcp "abacabacab" "bacabacab"       => 0 (no common prefix)
        llcp "abacabacab" "acabacab"        => 1
        llcp "abacabacab" "cabacab"         => 0
        llcp "abacabacab" "abacab"          => 6
        llcp "abacabacab" "bacab"           => 0
        llcp "abacabacab" "acab"            => 1
        llcp "abacabacab" "cab"             => 0
        llcp "abacabacab" "ab"              => 2
        llcp "abacabacab" "b"               => 0
[10,0,1,0,6,0,1,0,2,0]
```

## The Initial Program

The initial version is given as:

```haskell
allcp Eq a => [a] -> [Int]
allcp xs = fst4 (until (done n) (step xs) ([n], 0, 0, 1))
  where 
    n = length xs

done :: Eq a => a -> (t, t1, t2, a) -> Bool
done n (as, i, p, k) = k == n

step :: Eq a => [a] -> ([Int], Int, Int, Int) -> ([Int], Int, Int, Int)
step xs (as, i, p, k)
    | k >= i + p    = (snoc as a, k, a, k + 1)
    | q /= r        = (snoc as (min q r ), i, p, k + 1)
    | q == r        = (snoc as b, k, b, k + 1)
  where 
    q = as !! (k - i)
    r = p - (k - i)
    a = llcp xs (drop k xs)
    b = q + llcp (drop q xs) (drop (q + k) xs)

fst4 :: (a, b, c, d) -> a
fst4 (a, b, c, d) = a

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

llcp :: Eq a => [a] -> [a] -> Int
llcp xs [] = 0
llcp [] ys = 0
llcp (x:xs) (y:ys) = if x == y then 1 + llcp xs ys else 0
```

Code in chap15a.hs.

Testing it:

```haskell
ghci> let xs = "abacabacab"
ghci> allcp xs
[10,0,1,0,6,0,1,0,2,0]
```

## The Final Program

The final version of the code uses a queuing mechanism, where:

```haskell
insert :: Queue a -> a -> Queue a
remove :: Queue a -> (a, Queue a)
empty :: Queue a
elems :: Queue a -> [a]
```

We'll use an implementation of Okasaki's queue, taken from 
a (presentation by Andres Loeh) [https://www.andres-loeh.de/afp-ds-1.pdf].

```haskell
module Queue
where

data Queue a = Q !Int [a] !Int [a]
               deriving Show

empty :: Queue a
empty = Q 0 [] 0 []

head :: Queue a -> a
head (Q lf (f:fs) lb bs) = f
head (Q _ [] _ _) = error "empty queue"

tail :: Queue a -> Queue a
tail (Q lf (f:fs) lb bs) = makeQ (lf - 1) fs lb bs
tail (Q _ [] _ _) = error "empty queue"

snoc :: a -> Queue a -> Queue a
snoc x (Q lf fs lb bs) = makeQ lf fs (lb + 1) (x:bs)

makeQ :: Int -> [a] -> Int -> [a] -> Queue a
makeQ lf fs lb bs
    | lf > lb       = Q lf fs lb bs
    | otherwise     = Q (lf + lb) (fs ++ reverse bs) 0 []

elemsQueue :: Queue a -> [a]
elemsQueue (Q lf fs lb bs) = fs ++ (reverse bs)

insert = flip snoc

remove :: Queue a -> (a, Queue a)
remove (Q lf (f:fs) lb bs) = (f, makeQ (lf - 1) fs lb bs)
remove (Q _ [] _ _) = error "empty queue"
```

Code in Queue.hs.

> Note: We'll reference this module in chap15b.hs. To get this working in GHCi, remember
> to set the `-i' flag to include the current directory as follows:

```haskell
ghci> :set -i.
```

Also, As we've seen before (chapter 1), the mathematical symbol ∨ is the boolean 'or' function. However, 
Haskell's `or` function takes an array of booleans:

```haskell
ghci> :t or
or :: [Bool] -> Bool
```

We have to use the `(||)` function instead.

The final code is in chap15b.hs:

```haskell
import Data.Array
import Queue

allcp xs = extract (until done step (as, empty, 0, 1))
  where
    extract (as, qs, h, k) = elemsQueue as
    done (as, qs, h, k) = (k == n)
    n = length xs
    as = insert empty n
    xa = listArray (0, n - 1) xs
    step (as, qs, h, k) 
        | k >= h    = (insert as a, insert as' a, k + a, k + 1)
        | q /= r    = (insert as m, insert qs' m, h, k + 1)
        | q == r    = (insert as b, insert as' b, k + b, k + 1)
          where 
            as' = snd (remove as)
            (q, qs') = remove qs
            r = h - k
            m = min q r
            a = llcp' 0 k
            b = q + llcp' q (q + k)
    llcp' :: Int -> Int -> Int
    llcp' j k 
        | j == n || k == n  = 0
        | xa!j == xa!k      = 1 + llcp' (j + 1) (k + 1)
        | otherwise         = 0
```

Let's test it:

```haskell
ghci> :set -i.      -- Remember to set this
ghci> let xs = "abacabacab"
ghci> allcp xs
[10,0,1,0,6,0,1,0,2,0]
```

