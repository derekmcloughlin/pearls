import Data.Array

testData :: [Int]
testData = [51, 38, 29, 51, 63, 38]

ranking1 :: [Int]
ranking1 = [3, 1, 3, 0, 1] 

ranking2 :: [Int]
ranking2 = [2, 0, 3, 4, 0]

shiftBy :: Int -> [Int] -> [Int]
shiftBy k rs = map (+ k) (drop k rs) ++ [k - 1, k - 2 .. 0]

applyUntil :: (a -> Bool) -> [a -> a] -> a -> a
applyUntil p (f:fs) x = if p x then x else applyUntil p fs (f x)

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

partition :: Ord a => [a] -> [[Int]]
partition = psort . zip [0..]

ranktails :: Ord a => [a] -> [Int]
ranktails = resort . concat . label . 
                applyUntil (all single) repartitions . partition

repartitions :: [[[Int]] -> [[Int]]]
repartitions = map repartition (iterate (* 2) 1)

repartition :: Int -> [[Int]] -> [[Int]]
repartition k iss = partition(zip rs (shiftBy k rs))
                    where rs = resort (concat (label iss))

single :: [a] -> Bool
single xs = length xs == 1

