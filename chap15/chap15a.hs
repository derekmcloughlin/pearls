allcp xs = fst4 (until (done n) (step xs) ([n], 0, 0, 1))
  where 
    n = length xs

done :: Eq a => a -> (t, t1, t2, a) -> Bool
done n (as, i, p, k) = k == n

step :: Eq a => [a] -> ([Int], Int, Int, Int) -> ([Int], Int, Int, Int)
step xs (as, i, p, k)
    | k >= i + p    = (snoc as a, k, a, k + 1)
    | q /= r        = (snoc as (min q r ), i, p, k + 1)
    | q == r        = (snoc as b, k, b, k + 1)
  where 
    q = as !! (k - i)
    r = p - (k - i)
    a = llcp xs (drop k xs)
    b = q + llcp (drop q xs) (drop (q + k) xs)

fst4 :: (a, b, c, d) -> a
fst4 (a, b, c, d) = a

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

llcp :: Eq a => [a] -> [a] -> Int
llcp xs [] = 0
llcp [] ys = 0
llcp (x:xs) (y:ys) = if x == y then 1 + llcp xs ys else 0
