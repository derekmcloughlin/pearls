unravels :: [a] -> [[[a]]]
unravels = foldr (concatMap . prefixes) [[]]

prefixes x [ ] = [[[x]]]
prefixes x (xs : xss) = [(x : xs) : xss] ++ map (xs :) (prefixes x xss)

up :: Ord a => [a] -> Bool
up [] = True
up [x] = True
up (x:y:xs) = x <= y && up (y:xs)

minBy :: Ord b => (a -> b) -> [a] -> a
minBy f = foldl1 (cmp f)

cmp :: Ord b => (a -> b) -> a -> a -> a
cmp f u v = if f u <= f v then u else v

upravels :: Ord a => [a] -> [[[a]]]
upravels = filter (all up) . unravels

supravel :: Ord a => [a] -> [[a]]
supravel = minBy length . upravels
