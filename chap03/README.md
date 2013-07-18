Chapter 3 - Improving on saddleback search
================================

Prelude
-------

These type of searches are on 2-D arrays where the values in the array are defined by a
monotonically increasing function along both the X and Y axes.

For example, the function f x y = x + y produces:

     X 0 1 2 3 4
    Y  
    0  0 1 2 3 4
    1  1 2 3 4 5
    2  2 3 4 5 6
    3  3 4 5 6 7
    4  4 5 6 7 8

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

    invert :: (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a, a)]
    invert f z = [(x, y) | x <- [0..z ], y <- [0..z ], f x  y == z ]
    
    ghci> invert f1 10
    [(0,10),(1,9),(2,8),(3,7),(4,6),(5,5),(6,4),(7,3),(8,2),(9,1),(10,0)]

    ghci> invert f2 219
    [(7,6),(43,3),(73,0)]

Code is in chap03a.hs.    

Theo's improvement
----------------

    invert' f z = [(x, y) | x <- [0..z], y <- [0..z - x], f x, y == z ]
