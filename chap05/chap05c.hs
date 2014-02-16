import Data.List
import Data.List.Ordered
import Data.Array

xs = [1, 3, 5, 7, 9]
ys = [20, 30, 40, 50, 60]

--xs = [7, 3, 1, 9, 5]
--ys = [40, 20, 30, 60, 50]

sortsums :: Ord a => Num a => [a] -> [a] -> [a]
sortsums xs ys = map fst (sortsubs xs (map negate ys))

sortsubs :: Ord a => Num a => [a] -> [a] -> [(a, (Integer, Integer))]
sortsubs xs ys = sortBy (cmp (mkArray xs ys)) (subs xs ys)

cmp a (x, (i, j)) (y, (k, l)) = compare (a ! (1, i, k)) (a ! (2, j, l))

subs :: Num a => [a] -> [a] -> [(a, (Integer, Integer))]
subs xs ys = [ (x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]

table :: Ord a => Num a => [a] -> [a] -> [(Integer, Integer, Integer)]
table xs ys = map snd (map (tag 1) xxs `my_merge` map (tag 2) yys) 
              where xxs = sortsubs xs xs
                    yys = sortsubs ys ys 

tag :: Integer -> (a, (Integer, Integer)) -> (a, (Integer, Integer, Integer))
tag i (x,(j,k)) = (x,(i,j,k))

-- Merge two sorted lists to produce a sorted list
my_merge :: Ord a => [a] -> [a] -> [a]
my_merge xs [] = xs
my_merge [] xs = xs
my_merge a@(h:first) b@(c:second)
        | h <= c = h:my_merge first b
        | h > c = c:my_merge a second

-- mkArray :: [a] -> [a] -> Array [a]
mkArray xs ys = array b (zip (table xs ys) [1..])
                where b = ((1, 1, 1), (2, p, p))
                      p = toInteger (max (length xs) (length ys)) 


