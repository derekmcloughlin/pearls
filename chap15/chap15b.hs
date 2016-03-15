import Data.Array
import Queue

allcp xs = extract (until done step (as, empty, 0, 1))
  where
    extract (as, qs, h, k) = elemsQueue as
    done (as, qs, h, k) = (k == n)
    n = length xs
    as = insert empty n
    xa = listArray (0, n - 1) xs
    step (as, qs, h, k) 
        | k >= h    = (insert as a, insert as' a, k + a, k + 1)
        | q /= r    = (insert as m, insert qs' m, h, k + 1)
        | q == r    = (insert as b, insert as' b, k + b, k + 1)
          where 
            as' = snd (remove as)
            (q, qs') = remove qs
            r = h - k
            m = min q r
            a = llcp' 0 k
            b = q + llcp' q (q + k)
    llcp' :: Int -> Int -> Int
    llcp' j k 
        | j == n || k == n  = 0
        | xa!j == xa!k      = 1 + llcp' (j + 1) (k + 1)
        | otherwise         = 0

