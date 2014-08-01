import Data.Array

testData :: [Int]
testData = [51, 38, 29, 51, 63, 38]

ranking1 :: [Int]
ranking1 = [3, 1, 3, 0, 1] 

ranking2 :: [Int]
ranking2 = [2, 0, 3, 4, 0]

tails :: [a] -> [[a]]
tails [] = []
tails xs = xs : tails (tail xs)

rats :: Ord a => Int -> [a] -> [Int]
rats k = rank . map (take k) . tails

(<<) :: Ord a => [a] -> [a] -> [Int]
xs << ys = rank (zip xs ys)

shiftBy :: Int -> [Int] -> [Int]
shiftBy k rs = map (+k) (drop k rs) ++ [k-1, k-2 .. 0]

ranktails :: Ord a => [a] -> [Int]
ranktails = applyUntil isperm rerankings .rank

rerankings :: [[Int] -> [Int]]
rerankings = map rerank (iterate (*2) 1)

rerank :: Int -> [Int] -> [Int]
rerank k rs = rs << shiftBy k rs

applyUntil :: (a -> Bool) -> [a -> a] -> a -> a
applyUntil p (f:fs) x = if p x then x else applyUntil p fs (f x)

isperm :: [Int] -> Bool
isperm is = and (elems
            (accumArray (||) False (0, n - 1) (zip is (repeat True))))
            where n = length is


rank :: Ord a => [a] -> [Int]
rank = resort . concat . label . psort . zip [0..]

psort :: Ord b => [(a, b)] -> [[a]]
psort xys = pass xys []

pass [] xss = xss
pass (e@(x , y):xys) xss = step xys [] [x] [] xss
    where
        step [] as bs cs xss = pass as (bs : pass cs xss)
        step (e@(x, y'):xys) as bs cs xss 
            | y' < y    = step xys (e:as) bs cs xss
            | y' == y   = step xys as (x:bs) cs xss
            | y' > y    = step xys as bs (e:cs) xss


label :: [[a]] -> [[(a, Int)]]
label xss = zipWith tag xss (scanl (+) 0 (map length xss))

tag :: [a] -> b -> [(a, b)]
tag xs k = [(x, k) | x <- xs]

resort :: [(Int, Int)] -> [Int]
resort ijs = elems (array (0, length ijs - 1) ijs)


