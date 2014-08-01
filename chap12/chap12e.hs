import Data.Array

testData :: [Int]
testData = [51, 38, 29, 51, 63, 38]

ranking1 :: [Int]
ranking1 = [3, 1, 3, 0, 1] 

ranking2 :: [Int]
ranking2 = [2, 0, 3, 4, 0]

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

single :: [a] -> Bool
single xs = length xs == 1

ranktails :: Ord a => [a] -> [Int]
ranktails xs = (resort n . concat . label .
               applyUntil (all single) (repartitions n) .
               psort . zip [0..]) xs
               where n = length xs

resort :: Int -> [(Int, Int)] -> [Int]
resort n = elems . array (0, n - 1)

label :: [[a]] -> [[(a, Int)]]
label iss = zipWith tag iss (scanl (+) 0 (map length iss))

tag :: [a] -> b -> [(a, b)]
tag is j = [(i, j ) | i <- is]

repartitions :: Int -> [[[Int]] -> [[Int]]]
repartitions n = map (repartition n) (iterate (* 2) 1)

repartition :: Int -> Int -> [[Int]] -> [[Int]]
repartition n k iss = concatMap (psort . map install ) iss
                      where install i = (i, if j < n then k + a!j else n - i - 1)
                                        where j = i + k
                            a = array (0, n - 1) (concat (label iss))



