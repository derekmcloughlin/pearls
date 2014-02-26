Chapter 8 - Unravelling Greedy Algorithms
=========================================

Introduction
------------

Unravelling an array:

```haskell
unravels :: [a] -> [[[a]]]
unravels = foldr (concatMap . prefixes) [[]]

prefixes x [ ] = [[[x]]]
prefixes x (xs : xss) = [(x : xs) : xss] ++ map (xs :) (prefixes x xss)

ghci> unravels "bart"
[["bart"],["art","b"],["brt","a"],["rt","ba"],["rt","a","b"],["bat","r"],["at","br"],["at","r","b"],["bt","ar"],
 ["t","bar"],["t","ar","b"],["bt","r","a"],["t","br","a"],["t","r","ba"],["t","r","a","b"]]
```

For upravels, the definition of `up` isn't given in the book, but it's straightforward:

```haskell
up :: Ord a => [a] -> Bool
up [] = True
up [x] = True
up (x:y:xs) = x <= y && up (y:xs)
```

With this we have `upravels`. 

```haskell
upravels :: Ord a => [a] -> [[[a]]]
upravels = filter (all up) . unravels

ghci> upravels "bart"
[["art","b"],["brt","a"],["rt","a","b"],["at","br"],["at","r","b"],["bt","ar"],["t","ar","b"],
 ["bt","r","a"],["t","br","a"],["t","r","a","b"]]
```

Finally we find the shortest upravel:

```haskell
-- Same as chap07
minBy :: Ord b => (a -> b) -> [a] -> a
minBy f = foldl1 (cmp f)

-- Same as chap07
cmp :: Ord b => (a -> b) -> a -> a -> a
cmp f u v = if f u <= f v then u else v

supravel :: Ord a => [a] -> [[a]]
supravel = minBy length . upravels

ghci> supravel "bart"
["art","b"]
```

Code in chap08a.hs.



Derivation
----------

An alternative version of `upravels` using foldr is as follows:

```haskell
upravels :: Ord a => [a] -> [[[a]]]
upravels = foldr (concatMap . uprefixes) [[]]

uprefixes x [] = [[[x]]]
uprefixes x (xs : xss) = if x <= head xs then
                            [(x : xs) : xss] ++ map (xs :) (uprefixes x xss)
                         else map (xs :) (uprefixes x xss)
```

Code in chap08b.hs.

