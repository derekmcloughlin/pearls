supravel :: Ord a => [a] -> [[a]]
supravel = minBy length . filter (all up) . unravels

minBy :: Ord b => (a -> b) -> [a] -> a
minBy f = foldl1 (cmp f)

cmp :: Ord b => (a -> b) -> a -> a -> a
cmp f u v = if f u <= f v then u else v
