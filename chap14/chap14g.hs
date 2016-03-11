import Data.List

maxtail :: Ord a => [a] -> [a]
maxtail = foldl op []

op :: Ord a => [a] -> a -> [a]
op ys x = maximum [zs ++ [x] | zs <- borders ys]

-- Borders

borders :: Ord a => [a] -> [[a]]
borders [] = [[]]
borders xs = xs : borders (border xs)

-- Border

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
    | xs == []                  = []
    | length(xs) == 1           = []
    | ys_after_zs == []         = []
    | head(ys_after_zs) < x     = border (zs ++ [x])
    | head(ys_after_zs) == x    = (zs ++ [x])
    | head(ys_after_zs) > x     = border (zs ++ [x])
  where 
    ys = init xs 
    x = last xs 
    zs = border ys 
    ys_after_zs = (ys `after` zs) 

-- Cocktail

maxtail' :: Ord a => [a] -> [a]
maxtail' = uncurry (++) . cocktail'

cocktail' :: Ord a => [a] -> ([a], [a])
cocktail' = foldl op' ([], [])

op' :: Ord a => ([a], [a]) -> a -> ([a], [a])
op' (zs, ws) x 
    | null ws   = ([], [x])
    | w < x     = cocktail' (zs ++ [x])
    | w == x    = (zs ++ [x], tail ws ++ [x])
    | w > x     = ([], zs ++ ws ++ [x])
  where 
    w = head ws


-- Reducing the problem size

maxtail'' :: Ord a => [a] -> [a]
maxtail'' = uncurry (++) . cocktail''

cocktail'' :: Ord a => [a] -> ([a], [a])
cocktail'' = foldl op'' ([], [])

op'' (zs, ws) x 
    | null ws   = ([], [x])
    | w < x     = cocktail'' (take r zs ++ [x])
    | w == x    = (zs ++ [x], tail ws ++ [x])
    | w > x     = ([], zs ++ ws ++ [x])
  where 
    w = head ws
    r = (length zs) `mod` (length ws)


-- Final optimisations

maxtail''' :: Ord a => [a] -> [a]
maxtail''' [] = []
maxtail''' (x:xs) = step (0, 1, x:xs, x:xs, xs)

step (p, q, ys, ws, []) = ys
step (p, q, ys, w : ws, x:xs)
    | w < x     = maxtail''' (drop (q - r) (w:ws))
    | w == x    = step (p + 1, q, ys, ws, xs)
    | w > x     = step (0, p + q + 1, ys, ys, xs)
    where r = p `mod` q

