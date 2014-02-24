Chapter 7 - Building a Tree with Minimum Height
===============================================

Leaf-labelled Trees
-------------------

The data type for a leaf-labelled tree is:

```haskell
data Tree = Leaf Int |
            Fork Tree Tree
            deriving (Show)
```

An example of such a tree is:

```haskell
{-
     tree_1
       /\
      /  \
    3     5
-}

tree_1 :: Tree
tree_1 = Fork (Leaf 3) (Leaf 5)
```

A more complicated example is:

```haskell
{-
             tree_2
               /\
              /  \
             /    \
            /      \
           X        X
          / \      / \
         /   \    /   \
        X     7  9     X
       / \            / \
      8   2          6   X
                        / \
                       3   5
-}

tree_2 :: Tree
tree_2 = Fork 
            (Fork 
                (Fork (Leaf 8) (Leaf 2))
                (Leaf 7))
            (Fork 
                (Leaf 9)
                (Fork (Leaf 6) 
                    (Fork (Leaf 3) (Leaf 5))))
```

The fringe of a tree is the list of integers at the leaf: (note: this code isn't in the book)

```haskell
fringe :: Tree -> [Int]
fringe (Leaf x) = [x]
fringe (Fork u v) = fringe u ++ fringe v
```

The cost of a tree is defined as:

```haskell
cost :: Tree -> Int
cost (Leaf x) = x
cost (Fork u v) = 1 + max (cost u) (cost v)
```

Trying this on our two trees:

```haskell
ghci> fringe tree_1
[3, 5]
ghci> cost tree_1
6
ghci> fringe tree_2
[8,2,7,9,6,3,5]
ghci> cost tree_2
11
```

To build a list of all possible trees with a given fringe:

```haskell
trees :: [Int] -> [Tree]
trees [x] = [Leaf x]
trees (x:xs) = concatMap (prefixes x) (trees xs)

prefixes :: Int -> Tree -> [Tree] 
prefixes x t@(Leaf y)   = [Fork (Leaf x) t]
prefixes x t@(Fork u v) = [Fork (Leaf x) t] ++
                          [Fork u' v | u' <- prefixes x u]
```

Testing in ghci:

```haskell
ghci> trees [3, 5]          # tree_1
[Fork (Leaf 3) (Leaf 5)]    # This is the only possible tree with this fringe

ghci> trees [8, 2, 7, 9, 6, 3, 5]   # tree_2
.....

ghci> length $ trees [8, 2, 7, 9, 6, 3, 5]
132
```

Code in chap07a.hs.


Using a Fold Function
---------------------

By defining a fold function `foldrn` that can work on non-empty lists, we can re-write
`trees`:

```haskell
foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b 
foldrn f g [x]      = g x
foldrn f g (x:xs)   = f x (foldrn f g xs)

trees :: [Int] -> [Tree]
trees = foldrn (concatMap . prefixes) (wrap . Leaf)
wrap x = [x]
```

Code in chap07b.hs. You can check that the definitions produce the same results.


Introducting Forests
--------------------

A forest is a list of trees:

```haskell
type Forest = [Tree]
```

We can redefine `trees` as follows:

```haskell
trees :: [Int] -> [Tree]
trees = map rollup . forests

forests :: [Int ] -> [Forest]
forests = foldrn (concatMap . prefixes) (wrap . wrap . Leaf )

wrap :: a -> [a]
wrap x = [x]

prefixes :: Int -> Forest -> [Forest ] 
prefixes x ts = [Leaf x : rollup (take k ts) : drop k ts | k <- [1 .. length ts]]

rollup :: Forest -> Tree 
rollup = foldl1 Fork
```

Code in chap07c.hs. The definitions produce the same results, but in a different order 
        
```haskell
ghci> :l chap07a.hs
ghci> trees [1, 2, 3, 4]
[Fork (Leaf 1) (Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4))),
 Fork (Fork (Leaf 1) (Leaf 2)) (Fork (Leaf 3) (Leaf 4)),
 Fork (Leaf 1) (Fork (Fork (Leaf 2) (Leaf 3)) (Leaf 4)),
 Fork (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) (Leaf 4),
 Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)]

ghci> :l chap07c.hs
ghci> trees [1, 2, 3, 4]
[Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4),
 Fork (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) (Leaf 4),
 Fork (Leaf 1) (Fork (Fork (Leaf 2) (Leaf 3)) (Leaf 4)),
 Fork (Fork (Leaf 1) (Leaf 2)) (Fork (Leaf 3) (Leaf 4)),
 Fork (Leaf 1) (Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4)))]
 ```

Calculating the Minimum Cost
----------------------------

```haskell
minBy :: Ord b => (a -> b) -> [a] -> a
minBy f = foldl1 (cmp f)

cmp :: Ord b => (a -> b) -> a -> a -> a
cmp f u v = if f u <= f v then u else v
```

Trying this out in ghci:

```haskell
ghci> minBy cost $ trees [1, 2, 3, 4, 5, 6]
Fork 
    (Fork 
        (Fork 
            (Fork 
                (Fork (Leaf 1) (Leaf 2)) 
                (Leaf 3)) 
            (Leaf 4))
        (Leaf 5)) 
    (Leaf 6)
```

Code in chap07d.hs.

