import Data.List
import Data.List.Ordered
import Data.Array

xs = [7, 3, 1, 9, 5]
ys = [40, 20, 30, 60, 50]

sortsums :: Ord a => Num a => [a] -> [a] -> [a]
sortsums xs ys = map fst (sortsubs xs (map negate ys))

sortsubs :: Ord a => Num a => [a] -> [a] -> [(a, (Integer, Integer))]
sortsubs xs ys = sort (subs xs ys)

subs :: Num a => [a] -> [a] -> [(a, (Integer, Integer))]
subs xs ys = [ (x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]
