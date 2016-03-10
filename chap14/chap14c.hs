import Data.List

maxtail :: Ord a => [a] -> [a]
maxtail = foldl op []

op :: Ord a => [a] -> a -> [a]
op ys x = maximum [zs ++ [x] | zs <- borders ys]

after :: Eq a => [a] -> [a] -> [a]
after [] ys = ys
after xs [] = xs
after (x:xs) (y:ys) 
    | x == y = after xs ys
    | otherwise = (x:xs)

before :: Eq a => [a] -> [a] -> [a]
before xs ys = reverse $ after (reverse xs) (reverse ys)

border :: Eq a => [a] -> [a]
border xs
    | xs == []                  = []
    | length(xs) == 1           = []
    | head (ys `after` zs) == x = zs ++ [x]
    | otherwise                 = border (zs ++ [x])
  where 
    ys = init xs 
    x = last xs 
    zs = border ys

borders :: Eq a => [a] -> [[a]]
borders [] = [[]]
borders xs = xs : borders (border xs)
