-- The Smallest Free Number

-- Array-based Solution

import Data.List

minfree :: [Integer] -> Integer

minfree xs = minfrom 0 (toInteger (length xs), xs)

minfrom :: Integer -> (Integer, [Integer]) -> Integer

minfrom a (n, xs)   | n == 0        = a
                    | m == b - a    = minfrom b (n - m, vs)
                    | otherwise     = minfrom a (m, us)
                      where (us, vs)    = partition (< b) xs
                            b           = a + 1 + n `div` 2
                            m           = toInteger (length us)

