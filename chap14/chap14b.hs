import Data.List

maxtail :: Ord a => [a] -> [a]
maxtail = foldl op []

op :: Ord a => [a] -> a -> [a]
op ys x = maximum [zs ++ [x] | zs <- borders ys]

-- Borders

borders :: Eq a => [a] -> [[a]]
borders xs = [ys | ys <- tails xs, ys `isPrefixOf` xs]
