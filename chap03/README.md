Chapter 3 - Improving on saddleback search
================================

Prelude
-------

These type of searches are on 2-D arrays where the values in the array are defined by a
monotonically increasing function along both the X and Y axes.

For example, the function f x y = x + y produces:

    4  4 5 6 7 8
    3  3 4 5 6 7
    2  2 3 4 5 6
    1  1 2 3 4 5
    0  0 1 2 3 4
    Y  
     X 0 1 2 3 4

The lowest value is always in one corner and the higest value in the opposite corner.

For testing purposes we define some useful functions:

    -- Make a 2-D array of size z given a function f
    mkArray :: (Enum a, Num a) => (a -> a -> a) -> a -> [[a]]
    mkArray f z = [ [f x y | x <- [0..z]] | y <- [0..z]]

    -- Stringify a 1-D array
    formatArray :: (Show a, Num a) => [a] -> String
    formatArray [] = ""
    formatArray (x:xs) = show x ++ "\t" ++ formatArray xs

    -- Stringify a 2-D array
    format2DArray :: (Show a, Num a) => [[a]] -> String
    format2DArray [] = ""
    format2DArray (x:xs) = formatArray x ++ "\n" ++ format2DArray xs

    -- In this function, all values from 0..z are represented   
    f1 :: Num a => a -> a -> a
    f1 x y = x + y

    -- In this function, z may not be represented at all
    f2 :: Num a => a -> a -> a
    f2 x y = 3*x + 27*y + y*y

Jack's first stab
----------------

The brute-force approach:

    invert :: (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a, a)]
    invert f z = [(x, y) | x <- [0..z ], y <- [0..z ], f x y == z ]
    
    ghci> invert f1 10
    [(0,10),(1,9),(2,8),(3,7),(4,6),(5,5),(6,4),(7,3),(8,2),(9,1),(10,0)]

    ghci> invert f2 219
    [(7,6),(43,3),(73,0)]

Code is in chap03a.hs.    

Theo's improvement
----------------

Only look for items below the diagonal:

    invert_b :: (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a, a)]
    invert_b f z = [(x, y) | x <- [0..z], y <- [0..z - x], f x y == z ]

    ghci> invert_b f1 10
    [(0,10),(1,9),(2,8),(3,7),(4,6),(5,5),(6,4),(7,3),(8,2),(9,1),(10,0)]

    ghci> invert_b f2 219
    [(7,6),(43,3),(73,0)]


Anne reduces it further
-----------------------

Firstly, redefine the way the search is done:

    find :: (Enum t, Eq t, Num t) => (t, t) -> (t -> t -> t) -> t -> [(t, t)]
    find (u, v) f z = [(x, y) | x <- [u .. z ], y <- [v, v - 1..0], f x y == z]

    invert_c :: (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a, a)]
    invert_c f z = find (0, z) f z
    
    ghci> invert_c f1 10
    [(0,10),(1,9),(2,8),(3,7),(4,6),(5,5),(6,4),(7,3),(8,2),(9,1),(10,0)]

    ghci> invert_c f2 219
    [(7,6),(43,3),(73,0)]


Now we find a more efficient version of find:

    find_d :: (Ord t, Enum t, Eq t, Num t) => (t, t) -> (t -> t -> t) -> t -> [(t, t)]
    find_d (u, v) f z
        | u > z || v < 0   = []
        | z' < z           = find_d (u + 1, v) f z
        | z' == z          = (u, v) : find_d (u + 1, v - 1) f z
        | z' > z           = find_d (u, v - 1) f z
        where z' = f u v

    invert_d :: (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a, a)]
    invert_d f z = find_d (0, z) f z

    ghci> invert_d f1 10
    [(0,10),(1,9),(2,8),(3,7),(4,6),(5,5),(6,4),(7,3),(8,2),(9,1),(10,0)]

    ghci> invert_d f2 219
    [(7,6),(43,3),(73,0)]

A Quick Note on Typeclasses
---------------------------
Some of the typeclasses used in the signatures are a bit messy - I just kept adding in extra
ones when the compiler complained about something.

However, typeclasses have a [hierarchy]( http://blogs.msdn.com/b/saeed/archive/2009/03/14/haskell-class-hierarchy-diagram.aspx), and 
looking at the hierarchy diagram you see that being a `Num` implies being an `Eq`. An `Integral` has all the
properties we need, so let's use it.

The type signature for `find_d` can be changed to:

    find_d :: (Integral) => (t, t) -> (t -> t -> t) -> t -> [(t, t)]

Changes with cleaned-up type sigs in chapt3b.hs.    

Theo's improvement 
------------------

    bsearch :: (Integral a) => (a -> a) -> (a, a) -> a -> a
    bsearch g (a, b) z
        | a + 1 == b    = a
        | g m <= z      = bsearch g (m, b) z
        | otherwise     = bsearch g (a, m) z
        where m = (a + b) `div` 2

    find_e :: (Integral a) => (a, a) -> (a -> a -> a) -> a -> [(a, a)]
    find_e (u, v) f z
        | u > n || n < 0   = []
        | z' < z           = find_e (u + 1, v) f z
        | z' == z          = (u, v) : find_e (u + 1, v - 1) f z
        | z' > z           = find_e (u, v - 1) f z
        where z' = f u v
              n = maximum (filter (\x -> f x 0 <= z) [0 .. z ])

    invert_e :: (Integral a) => (a -> a -> a) -> a -> [(a, a)]
    invert_e f z = find_e (0, m) f z
        where m = bsearch (\y -> f 0 y) (-1, z + 1) z

Code in chap03c.hs.

Final Version
-------------

    bsearch :: (Integral a) => (a -> a) -> (a, a) -> a -> a
    bsearch g (a, b) z
        | a + 1 == b    = a
        | g m <= z      = bsearch g (m, b) z
        | otherwise     = bsearch g (a, m) z
        where m = (a + b) `div` 2

    find_f :: (Integral a) => (a, a) -> (a, a) -> (a -> a -> a) -> a -> [(a, a)]
    find_f (u, v) (r, s) f z
        | u > r || v < s    = []
        | v - s <= r - u    = rfind (bsearch (\x -> f x q) (u - 1, r + 1) z)
        | otherwise         = cfind (bsearch (\y -> f p y) (s - 1, v + 1) z)
        where p = (u + r) `div` 2
              q = (v + s) `div` 2
              rfind p = (if f p q == z then (p, q) : find_f (u, v) (p-1, q+1) f z
                            else find_f (u, v) (p, q+1) f z) ++
                        find_f (p+1, q-1) (r , s) f z
              cfind q = find_f (u, v) (p-1, q+1) f z ++
                        (if f p q == z then(p, q) : find_f (p+1, q-1) (r , s) f z
                            else find_f (p+1, q) (r , s) f z)

    invert_f :: (Integral a) => (a -> a -> a) -> a -> [(a, a)]
    invert_f f z = find_f (0, m) (n, 0) f z
        where m = bsearch (\y -> f 0 y) (-1, z + 1) z
              n = bsearch (\x -> f x 0) (-1, z + 1) z

    ghci> invert_f f1 10
    [(5,5),(2,8),(1,9),(0,10),(4,6),(3,7),(8,2),(7,3),(6,4),(10,0),(9,1)]
    ghci> invert_f f2 219
    [(43,3),(7,6),(73,0)]

    -- Note that the ordering of the results is different.

Code in chap03d.hs.

Testing
-------

How do we go about testing the performance?

References
----------
The name saddleback search was given by [David Gries](http://www.cs.geneseo.edu/~baldwin/math-thinking/saddleback.html)

