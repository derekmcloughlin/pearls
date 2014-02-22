import Data.List

xs = [7, 3, 1, 9, 5]
ys = [40, 20, 30, 60, 50]

cmp :: Ord a => Num a => a -> a -> Ordering
cmp x y = compare x y

main = putStrLn $ show $ sortBy cmp [x + y | x <- xs, y <- ys]

