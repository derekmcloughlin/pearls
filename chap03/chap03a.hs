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

-- In this function, each value is represented only once.
f2 :: Num a => a -> a -> a
f2 x y = 3*x + 27*y + y*y


invert :: (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a, a)]
invert f z = [(x, y) | x <- [0..z ], y <- [0..z ], f x  y == z ]


invert' :: (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a, a)]
invert' f z = [(x, y) | x <- [0..z], y <- [0..z - x], f x y == z ]
