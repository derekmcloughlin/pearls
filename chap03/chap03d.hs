-- Make a 2-D array of size z given a function f
mkArray :: (Integral a) => (a -> a -> a) -> a -> [[a]]
mkArray f z = [ [f x y | x <- [0..z]] | y <- [0..z]]

-- Stringify a 1-D array
formatArray :: (Show a, Integral a) => [a] -> String
formatArray [] = ""
formatArray (x:xs) = show x ++ "\t" ++ formatArray xs

-- Stringify a 2-D array
format2DArray :: (Show a, Integral a) => [[a]] -> String
format2DArray [] = ""
format2DArray (x:xs) = formatArray x ++ "\n" ++ format2DArray xs

-- In this function, all values from 0..z are represented   
f1 :: Integral a => a -> a -> a
f1 x y = x + y

-- In this function, each value is represented only once.
f2 :: Integral a => a -> a -> a
f2 x y = 3*x + 27*y + y*y

-- Jack's first solution
invert :: (Integral a) => (a -> a -> a) -> a -> [(a, a)]
invert f z = [(x, y) | x <- [0..z ], y <- [0..z ], f x  y == z]

-- Theo's slight improvement
invert_b :: (Integral a) => (a -> a -> a) -> a -> [(a, a)]
invert_b f z = [(x, y) | x <- [0..z], y <- [0..z - x], f x y == z]

-- Anne reduces it further
find_c :: (Integral a) => (a, a) -> (a -> a -> a) -> a -> [(a, a)]
find_c (u, v) f z = [(x, y) | x <- [u .. z ], y <- [v, v - 1..0], f x y == z]

invert_c :: (Integral a) => (a -> a -> a) -> a -> [(a, a)]
invert_c f z = find_c (0, z) f z

-- And more efficiently
find_d :: (Integral a) => (a, a) -> (a -> a -> a) -> a -> [(a, a)]
find_d (u, v) f z
    | u > z || v < 0   = []
    | z' < z           = find_d (u + 1, v) f z
    | z' == z          = (u, v) : find_d (u + 1, v - 1) f z
    | z' > z           = find_d (u, v - 1) f z
    where z' = f u v

invert_d :: (Integral a) => (a -> a -> a) -> a -> [(a, a)]
invert_d f z = find_d (0, z) f z

-- Theo's improvement
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

-- Final version
find_f :: (Integral a) => (a, a) -> (a, a) -> (a -> a -> a) -> a -> [(a, a)]
find_f (u, v) (r, s) f z
    | u > r || v < s    = []
    | v - s <= r - u    = rfind (bsearch (\x -> f x q) (u - 1, r + 1) z)
    | otherwise         = cfind (bsearch (\y -> f p y) (s - 1, v + 1) z)
    where p = (u + r) `div` 2
          q = (v + s) `div` 2
          rfind p = (if f p q == z then (p, q) : find_f (u, v) (p - 1, q + 1) f z
                        else find_f (u, v) (p, q + 1) f z) ++
                    find_f (p + 1, q - 1) (r , s) f z
          cfind q = find_f (u, v) (p - 1, q + 1) f z ++
                    (if f p q == z then(p, q) : find_f (p + 1, q - 1) (r , s) f z
                        else find_f (p + 1, q) (r , s) f z)

invert_f :: (Integral a) => (a -> a -> a) -> a -> [(a, a)]
invert_f f z = find_f (0, m) (n, 0) f z
    where m = bsearch (\y -> f 0 y) (-1, z + 1) z
          n = bsearch (\x -> f x 0) (-1, z + 1) z


