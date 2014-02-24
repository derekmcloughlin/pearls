Chapter 6 - Making a Century
============================

Initial Version
---------------

The data types used break down an expression into a 3-D array of integers:

```haskell
type Expression = [Term]
type Term = [Factor] 
type Factor = [Digit] 
type Digit = Int
```

The factors are the digits from 1 .. 9:

```haskell
digits :: Factor
digits = [1 .. 9]
```

Here's how one expression is represented:

100 = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 * 9

```haskell
[[[1]],[[2]],[[3]],[[4]],[[5]],[[6]],[[7]],[[8],[9]]]
```

It's a bit difficult to see this, so reformatting:

```haskell
[
    [
        [1]
    ],
    [
        [2]
    ],
    [
        [3]
    ],
    [
        [4]
    ],
    [
        [5]
    ],
    [
        [6]
    ],
    [
        [7]
    ],
    [
        [8],[9]
    ]
]
```

The approach to find all ways to use the digits 1..9 to produce 100 evaluates
all possible expressions in a brute-force way. Evaluating an expression goes down the
tree from Expression to Term to Factor.

```haskell
valExpr :: Expression -> Int 
valExpr = sum . map valTerm

valTerm :: Term -> Int
valTerm = product . map valFact 

valFact :: Factor -> Int
valFact = foldl1 (\n d -> 10 * n + d)
```

Running this on our expression above:

```haskell
ghci> let e = [[[1]],[[2]],[[3]],[[4]],[[5]],[[6]],[[7]],[[8],[9]]] :: [[[Int]]]
ghci> valExpr e
100
```

Note that the type of `e` has to be represented as `[[[Int]]]` instead of being left 
unspecified, because the default type would be `[[[Integer]]]`.

The full list of expressions is calculated thus:

```haskell
expressions :: [Digit] -> [Expression] 
expressions = concatMap partitions . partitions
```

The "standard" function `partitions` isn't standard at all. Hoogle gives no 
function with a type:

```haskell
partitions :: [a] -> [[[a]]]
```

A quick internet search yields the following implementation
in the [Haskell Beginners](http://www.haskell.org/pipermail/beginners/2011-April/006832.html) thread:

```haskell
partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs]
                 ++ [(x:ys):yss | (ys:yss) <- partitions xs]
```

It works like this:

```haskell
ghci> partitions [1, 2, 3]
[[[1],[2],[3]],[[1],[2,3]],[[1,2],[3]],[[1,2,3]]]
```

Finally we define some functions to get us the expressions that evaluate to 100:

```haskell
good :: Int -> Bool
good v = v == 100

goodOnes :: [Digit] -> [Expression]
goodOnes = filter (good . valExpr) . expressions
```

Running this yields:

```haskell
ghci> goodOnes digits
[[[[1]],[[2]],[[3]],[[4]],[[5]],[[6]],[[7]],[[8],[9]]],
 [[[1],[2],[3]],[[4]],[[5]],[[6]],[[7]],[[8],[9]]],
 [[[1],[2],[3],[4]],[[5]],[[6]],[[7],[8]],[[9]]],
 [[[1]],[[2],[3]],[[4]],[[5]],[[6,7]],[[8]],[[9]]],
 [[[1],[2]],[[3,4]],[[5]],[[6],[7]],[[8]],[[9]]],
 [[[1,2]],[[3],[4]],[[5]],[[6]],[[7],[8]],[[9]]],
 [[[1,2]],[[3,4]],[[5],[6]],[[7]],[[8]],[[9]]]]
ghci> length $ goodOnes digits
7
```

Code is in chap06a.hs

Improving the Search
--------------------

First we use the 2nd version of the `expressions` function:

```haskell
expressions :: [Digit] -> [Expression] 
expressions = foldr extend []

extend :: Digit -> [Expression] -> [Expression] 
extend x [] = [[[[x]]]]
extend x es = concatMap (glue x) es

glue :: Digit -> Expression -> [Expression] 
glue x ((xs:xss):xsss) = [((x:xs):xss):xsss,
                          ([x] : xs : xss) : xsss, 
                          [[x]] : (xs : xss) : xsss]
```

Code in chap06b.hs. It gives the same results as the first version.

Next we have:

```haskell
cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) (x, y) = (f x, g y)

good c (_, f, t, e)  = (f * t + e ==c)
ok c (_, f, t, e)    = (f * t + e <= c)

modify x (k, f, t, e) = [(10 * k, k * x + f, t, e), (10, x, f * t, e), (10, x, 1, f * t + e)]

solutions :: Int -> [Digit] -> [Expression]
solutions c = map fst . filter (good c . snd) . foldr (expand c) []

expand c x [] = [([[[x]]], (10, x, 1, 0))]
expand c x evs = concat (map (filter (ok c . snd) . glue x) evs)

glue x ((xs : xss) : xsss, (k, f, t, e)) =
    [(((x : xs) : xss) : xsss, (10*k, k*x + f, t, e)),
    (([x] : xs : xss) : xsss, (10, x, f * t, e)),
    ([[x]] : (xs : xss) : xsss, (10, x, 1, f * t + e))]

```

This replaces the evaluation code (valFact, valTerm, valExpr).

Code in chap06c.hs.

Finally we can test the assertion that the new code is much faster, using 
100 as the number and the first 14 digits of Pi:

```haskell
pi_digits :: Factor
pi_digits = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7]

main = putStrLn $ "Number of solutions found: " ++ (show $ length $ solutions 1000 pi_digits)
```

We also put similar code in a modified version of our first solution:

```haskell
good v = v == 1000

main = putStrLn $ "Number of solutions found: " ++ (show $ length $ goodOnes pi_digits)
```

Code in chap06a-1.hs.

Not trusting performance testing directly in ghci, we compile both:

```
ghc -O2 -auto-all chap06a-1.hs
ghc -O2 -auto-all chap06c.hs
```

Running using the standard `time` program:

```
$ time ./chap06a-1
Number of solutions found: 202

real    0m1.649s
user    0m0.672s
sys     0m0.976s

$ time ./chap06c
Number of solutions found: 202

real    0m0.070s
user    0m0.000s
sys     0m0.068s
```

Yes, there's a huge difference.
