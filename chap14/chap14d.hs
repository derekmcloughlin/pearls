import Data.List
import Debug.Trace

debug = flip trace

maxtail :: Ord a => [a] -> [a]
maxtail = foldl op []

borders' xs = [ys | ys <- tails xs, ys `isPrefixOf` xs]

op :: Ord a => [a] -> a -> [a]
op ys x = maximum [zs ++ [x] | zs <- borders' ys]

after :: Eq a => [a] -> [a] -> [a]
after [] ys = ys
after xs [] = xs
after (x:xs) (y:ys) 
    | x == y = after xs ys
    | otherwise = (x:xs)

before :: Eq a => [a] -> [a] -> [a]
before xs ys = reverse $ after (reverse xs) (reverse ys)

border :: Ord a => [a] -> [a]
border xs 
    | xs == []                   = []
    | length(xs) == 1            = []
    | ys_after_zs == []          = []
    | head(ys_after_zs) < x      = border (zs ++ [x])
    | head(ys_after_zs) == x     = (zs ++ [x])
    | head(ys_after_zs) > x      = border (zs ++ [x])
  where 
    ys = init xs 
    x = last xs 
    zs = border ys 
    ys_after_zs = (ys `after` zs) 

borders :: (Show a, Ord a) => [a] -> [[a]]
borders [] = [[]]
borders xs = xs : borders (border xs)

