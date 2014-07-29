Chapter 12 - Ranking Suffixes
=============================

Specification
-------------

We introduce `tails` and `rank`:

```haskell
testData :: [Int]
testData = [51, 38, 29, 51, 63, 38]

tails :: [a] -> [[a]]
tails [] = []
tails xs = xs : tails (tail xs)

rank :: Ord a => [a] -> [Int]
rank xs = map (\x -> length (filter (< x) xs)) xs
```

The `tails` function differs from the one in `Data.List` in that the latter includes
the empty list.

```haskell
ghci> :l chap12a.hs 
ghci> tails testData
[[51,38,29,51,63,38],[38,29,51,63,38],[29,51,63,38],[51,63,38],[63,38],[38]]
ghci> Data.List.tails testData
[[51,38,29,51,63,38],[38,29,51,63,38],[29,51,63,38],[51,63,38],[63,38],[38],[]]
```

Ranking the list gives:


```haskell
ghci> rank testData
[3,1,0,3,5,1]
```

However, we want to rank the suffixes (or tails) of the list:

```haskell
ranktails :: Ord a => [a] -> [Int]
ranktails = rank . tails
```

```haskell
ghci> ranktails testData
[3,2,0,4,5,1]
```

In Haskell we can compare two arrays even if they have different lengths:

```haskell
ghci> [38,29,51,63,38] > [51,38,29,51,63,38]
False
ghci> [51, 63, 38] > [51,38,29,51,63,38] 
True
```

Code in chap12a.hs

