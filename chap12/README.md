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

A Better Algorithm
------------------

In looking at properties of the `rank` function, the following 'refining'
operator is introduced:

```haskell
(<<) :: Ord a => [a] -> [a] -> [Int]
xs << ys = rank (zip xs ys)
```

Note: In the book he has:

```haskell
[3, 1, 3, 0, 1] << [2, 0, 3, 4, 0] = [2, 1, 3, 0, 1]
```

This is incorrect. The real result is:

```haskell
ghci> [3, 1, 3, 0, 1] << [2, 0, 3, 4, 0]
[3,1,4,0,1]
```

The `<<` operator is associative, and has the following property: refining a
list that is a permutation of [0..n-1] with another list of length 'n' gives
the original list.

```haskell
ghci> [3, 5, 2, 0, 4, 1] << [2, 4, 5, 9, 1, 7]
[3, 5, 2, 0, 4, 1]
```

The function `rats` is introduced. `rats` only looks at the first `k` elements when
doing the comparison:

```haskell
rats :: Ord a => Int -> [a] -> [Int]
rats k = rank . map (take k) . tails
```

On our test data we have:

```haskell
ghci> rats 1 testData
[3,1,0,3,5,1]
ghci> rats 2 testData
[3,2,0,4,5,1]
ghci> rats 2 testData
[3,2,0,4,5,1]
ghci> rats 5 testData
[3,2,0,4,5,1]
```

The function `shiftBy` is used in definining another property of `rats`:

```haskell
shiftBy :: Int -> [Int] -> [Int]
shiftBy k rs = map (+k) (drop k rs) ++ [k-1, k-2 .. 0]
```

Example:

```haskell
ghci> shiftBy 3 [3, 1, 0, 3, 5, 1]
[6,8,4,2,1,0]
```

With those in place, the new `ranktails` function is here:

```haskell
ranktails :: Ord a => [a] -> [Int]
ranktails = applyUntil isperm rerankings .rank

rerankings :: [[Int] -> [Int]]
rerankings = map rerank (iterate (*2) 1)

rerank :: Int -> [Int] -> [Int]
rerank k rs = rs << shiftBy k rs

applyUntil :: (a -> Bool) -> [a -> a] -> a -> a
applyUntil p (f:fs) x = if p x then x else applyUntil p fs (f x)

isperm :: [Int] -> Bool
isperm is = and (elems (accumArray (||) False (0, n - 1) (zip is (repeat True))))
            where n = length is
```

```haskell
ghci> ranktails testData
[3,2,0,4,5,1]
```

Code in chap12b.hs

