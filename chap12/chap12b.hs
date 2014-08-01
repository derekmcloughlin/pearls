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

rank :: Ord a => [a] -> [Int]
rank xs = map (\x -> length (filter (< x) xs)) xs

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


