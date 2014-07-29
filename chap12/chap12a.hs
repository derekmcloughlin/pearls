testData :: [Int]
testData = [51, 38, 29, 51, 63, 38]

tails :: [a] -> [[a]]
tails [] = []
tails xs = xs : tails (tail xs)

rank :: Ord a => [a] -> [Int]
rank xs = map (\x -> length (filter (< x) xs)) xs

ranktails :: Ord a => [a] -> [Int]
ranktails = rank . tails

