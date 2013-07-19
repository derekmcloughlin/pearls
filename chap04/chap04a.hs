smallest :: Ord a => Int -> ([a], [a]) -> a
smallest k (xs, ys) = union (xs, ys) !! k


union ([], [ ]) = []
union (xs, [ ]) = xs
union ([ ], ys) = ys
union (x : xs, y : ys) 
    | x < y     = x : union (xs, y : ys)
    | x > y     = y : union (x : xs, ys)
