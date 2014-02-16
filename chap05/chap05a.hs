import Data.List

xs = [7, 3, 1, 9, 5]
ys = [40, 20, 30, 60, 50]
-- xs = [1, 3, 5, 7, 9]
-- ys = [20, 30, 40, 50, 60]

my_compare :: Ord a => Num a => a -> a -> Ordering
my_compare x y = compare x y

main = putStrLn $ show $ sortBy my_compare [x + y | x <- xs, y <- ys]

