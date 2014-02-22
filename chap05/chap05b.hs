import Data.List
import Data.List.Ordered
import Data.Array

-- xs = [7, 3, 1, 9, 5]
-- ys = [40, 20, 30, 60, 50]
xs = [1, 3, 5, 7, 9]
ys = [20, 30, 40, 50, 60]

my_compare :: Ord a => Num a => (a, (Int, Int)) -> (a, (Int, Int)) -> Ordering
my_compare x y = compare x y

sortsums :: Ord a => Num a => [a] -> [a] -> [a]
sortsums xs ys = map fst (sortsubs xs (map negate ys))

sortsubs :: Ord a => Num a => [a] -> [a] -> [(a, (Int, Int))]
sortsubs xs ys = sortBy my_compare (subs xs ys)

subs :: Ord a => Num a => [a] -> [a] -> [(a, (Int, Int))]
subs xs ys = [ (x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]

main = putStrLn $ show $ sortsums xs ys 

