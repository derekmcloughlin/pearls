import Data.List
import Data.Array

xs = [1, 3, 5, 7, 9]
ys = [20, 30, 40, 50, 60]

subs :: Num a => [a] -> [a] -> [(a, (Integer, Integer))]
subs xs ys = [ (x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]

sortsums :: Ord a => Num a => [a] -> [a] -> [a]
sortsums xs ys = map fst (sortsubs xs (map negate ys))

sortsubs :: Ord a => Num a => [a] -> [a] -> [(a, (Integer, Integer))]
sortsubs xs ys = sort (subs xs ys)

table :: Ord a => Num a => [a] -> [a] -> [(Integer, Integer, Integer)]
table xs ys = map snd (map (tag 1) xxs `merge` map (tag 2) yys) 
              where xxs = sortsubs xs xs
                    yys = sortsubs ys ys 

tag :: Integer -> (a, (Integer, Integer)) -> (a, (Integer, Integer, Integer))
tag i (x,(j,k)) = (x,(i,j,k))

-- Merge two sorted lists to produce a sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge a@(h:first) b@(c:second)
        | h <= c = h:merge first b
        | h > c = c:merge a second

-- mkArray :: [a] -> [a] -> Array [a]
mkArray xs ys = array b (zip (table xs ys) [1..])
                where b = ((1, 1, 1), (2, p, p))
                      p = toInteger (max (length xs) (length ys)) 


