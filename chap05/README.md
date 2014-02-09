Chapter 5 - Sorting Pairwise Sums
===============================

Lambert's Algorithm
-------------------

For the purposes of this chapter we need a monotonic function ⊕ and it's 
negation ⊖ . Let's use ordinary addition and subtraction for this.

The properties are satisfied:

    x <= x', y <= y' => x + y <= x' + y'
    x + y = x - negate y
    x - y <= x' - y' => x - x' <= y - y'

where negate can be the standard Haskell `negate` function:

```haskell
negate :: Num a => a -> a

ghci> negate 3
-3
ghci> negate (-1)
1
```

For testing:

```haskell
let xs = [1, 3, 5, 7, 9, 11]
let ys = [20, 30, 40, 50, 60, 70]
```



