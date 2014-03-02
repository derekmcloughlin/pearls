Chapter 10 - Removing Duplicates
================================

A First Version
---------------

The first version uses some functions that you can 
see in [chapter 1](https://github.com/derekmcloughlin/pearls/tree/master/chap01).

```haskell
notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = notElem n a

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

nub' :: Ord a => [a] -> [a]
nub' [] = []
nub' (x:xs) = if notElem x xs then x : nub' xs 
              else (x : nub' (xs \\ [x])) `min` (nub' xs)
```

I've also defined the function as `nub'` instead of just `nub` to avoid
confusion to the version in Data.List.

Testing it out:

```haskell
ghci> nub "calculus"
"calus"
ghci> nub' "calculus"
"aclus"
```

To make it easier to profile the code with different strings, let's include a main method:

```haskell
main = do
    args <- getArgs
    let word = case args of
                    [] -> "calculus"
                    (x:_) -> x
    putStrLn $ show $ nub' word
```

Code in chap10a.hs.

A Better Version
----------------

There's a lot of theory that goes into this, but the next version is as follows:

```haskell
nub'' :: Ord a => [a] -> [a]
nub'' = hub []

hub :: Ord a => [a] -> [a] -> [a]
hub ws []       = []
hub ws (x:xs)   = case (x `elem` xs, x `elem` ws) of
                    (False, False)  -> us ++ [x] ++ hub [] (xs\\us) 
                    (False,True)    -> us ++ [x] ++ hub (tail vs) (xs\\us) 
                    (True, False)   -> hub (us ++ [x]) xs
                    (True, True)    -> hub ws xs
                  where (us , vs) = span (< x) ws
```

Introducing Sets
----------------

Using `Data.Set` we can use the following definition of `nub'`:

```haskell
import Data.Set

nub' :: Ord a => [a] -> [a]
nub' = hub empty . preprocess

preprocess :: Ord a => [a] -> [(a, Set a)]
preprocess xs = zip xs (tail (scanr insert empty xs))

hub :: Ord a => Set a -> [(a, Set a)] -> [a]
hub ws [] = []
hub ws ((x,xs) : xss) = case (member x xs, member x ws) of
    (False,False)   -> eus ++ [x] ++ hub empty yss
    (False,True)    -> eus ++ [x] ++ hub vs yss
    (True,False)    -> hub (insert x us) xss
    (True,True)     -> hub ws xss
    where (us,vs) = split x ws
          eus = elems us
          yss = [(x, xs) | (x, xs) <- xss, not (member x us)]
```

Code in chap10c.hs.


Getting to a O(n log n) solution
--------------------------------

The final version uses `foldr`:


```haskell
nub' :: Ord a => [a] -> [a]
nub' = hub' empty empty . preprocess

preprocess :: Ord a => [a] -> [(a, Set a)]
preprocess xs = zip xs (tail (scanr insert empty xs))

hub' :: Ord a => Set a -> Set a -> [(a, Set a)] -> [a]
hub' ps ws [] = []
hub' ps ws ((x, xs) : xss) = 
    if member x ps then 
        hub' ps ws xss 
    else case (member x xs, member x ws) of
        (False, False)   -> eus ++ [x] ++ hub' qs empty xss
        (False, True)    -> eus ++ [x] ++ hub' qs vs xss
        (True, False)    -> hub' ps (insert x us) xss
        (True, True)     -> hub' ps ws xss
        where (us, vs) = split x ws
              eus = elems us
              qs = Prelude.foldr insert ps eus
```

Code in chap10d.hs.
