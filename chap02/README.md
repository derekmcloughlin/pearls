Chapter 2 - A Surpassing Problem
================================

A naïve solution
----------------

    msc xs = maximum [scount z zs | z : zs <- tails xs]

    scount x xs = length(filter(x < ) xs)

    tails [] = []
    tails (x:xs) = (x:xs) : tails xs

This is in chap02a.hs

Let's test it.

    ghci> :l chap02a.hs
    ghci> tails "generating"
    ["generating","enerating","nerating","erating","rating","ating","ting","ing","ng","g"]
    ghci> scount 'g' "enerating"
    5
    ghci> msc "generating"
    6

It works.

A Divide and Conquer Solution
-----------------------------

First, generating a list of the elements along with their surpasser count:

    ghci> let table xs = [(z,scountzzs) | z:zs <- tails xs]
    ghci> table "generating"
    [('g',5),('e',6),('n',2),('e',5),('r',1),('a',4),('t',0),('i',1),('n',0),('g',0)]
    ghci> let msc' xs = maximum $ map snd $ table xs

Note the use of `$` here instead of the `.` operator. We could also have:

    ghci> let msc' xs = maximum $ (map snd) (table xs)

or even better:

    ghci> let msc' xs = maximum . map snd $ table xs

but not:

    ghci> let msc' = maximum . map snd . table
    ghci> msc' "generating"

    <interactive>:53:6:
        Couldn't match expected type `()' with actual type `Char'
        Expected type: [()]
          Actual type: [Char]
        In the first argument of msc', namely `"generating"'
        In the expression: msc' "generating"

because:

    ghci> :t msc'
    msc' :: [()] -> Int

whereas we want

    ghci> :t msc'
    msc' :: Ord a => [a] -> Int

Code in chap02b.hs

Now for the finale:

    table [x] = [(x, 0)]

    table xs = join (m - n) (table ys) (table zs)
               where m = length xs
                     n = m `div` 2              -- Watch out for the backticks
                     (ys, zs) = splitAt n xs    

    join 0 txs []   = txs
    join n [] tys   = tys
    join n txs@((x, c) : txs') tys@((y, d) : tys')
        | x < y = (x, c + n) : join n txs' tys 
        | x >= y = (y, d) : join (n - 1) txs tys'

    msc xs = maximum . map snd $ table xs


And to test:

    ghci> :l chap02c.hs 
    [1 of 1] Compiling Main             ( chap02c.hs, interpreted )
    Ok, modules loaded: Main.
    ghci> table "generating"
    [('a',4),('e',5),('e',6),('g',0),('g',5),('i',1),('n',0),('n',2),('r',1),('t',0)]
    ghci> msc "generating"
    6


