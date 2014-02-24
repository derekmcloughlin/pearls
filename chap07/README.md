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


