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


