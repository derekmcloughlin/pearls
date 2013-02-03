import Data.Array

countlist :: [Integer] -> Array Integer Integer

countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1)) 
    where n = toInteger (length xs)
