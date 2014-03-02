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

