import Data.List
import Data.List.Ordered
import Data.Array

xs = [7, 3, 1, 9, 5]
ys = [40, 20, 30, 60, 50]

cmp :: Ord a => Num a => (a, (Int, Int)) -> (a, (Int, Int)) -> Ordering
cmp x y = compare x y

sortsums :: Ord a => Num a => [a] -> [a] -> [a]
sortsums xs ys = map fst (sortsubs xs (map negate ys))

sortsubs :: Ord a => Num a => [a] -> [a] -> [(a, (Int, Int))]
sortsubs xs ys = sortBy cmp (subs xs ys)

subs :: Ord a => Num a => [a] -> [a] -> [(a, (Int, Int))]
subs xs ys = [ (x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]

main = putStrLn $ show $ sortsums xs ys 

