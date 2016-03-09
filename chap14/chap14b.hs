import Data.List
import Debug.Trace

{-
maxtail :: Ord a => [a] -> [a]
maxtail = foldl op []

op ys x 
    | ys == []              = [x]
    | head (ys ? zs) >= x   = ys ++ [x ]
    | otherwise             = op zs x
  where zs = border ys
-}

{-
after :: Eq a => [a] -> [a] -> [a]
after xs ys = dropWhileEqual xs ys
  where
    dropWhileEqual :: Eq a => [a] -> [a] -> [a]
    dropWhileEqual [] ys =  ys
    dropWhileEqual xs [] = xs
    dropWhileEqual (x:xs) (y:ys) 
        | x == y = dropWhileEqual xs ys
        | otherwise = (x:xs)
-}

after :: Eq a => [a] -> [a] -> [a]
after [] ys = ys
after xs [] = xs
after (x:xs) (y:ys) 
    | x == y = after xs ys
    | otherwise = (x:xs)

before :: Eq a => [a] -> [a] -> [a]
before xs ys = reverse $ after (reverse xs) (reverse ys)

border :: (Show a, Ord a) => [a] -> [a]
border xs 
    | xs == []                   = []
    | length(xs) == 1            = xs
    | ys_after_zs == []          = []
    | head(ys_after_zs) < x      = trace("head < x") border (zs ++ [x])
    | head(ys_after_zs) == x     = trace("head == x") zs ++ [x]
    | head(ys_after_zs) > x      = trace("head > x") []
  where 
    ys = trace("ys = init " ++ (show xs)) init xs
    x = trace("x = last " ++ (show xs)) last xs
    zs = trace("zs = border " ++ (show ys)) border ys
    ys_after_zs = trace("ys_after_sz = " ++ (show ys) ++ " `after` " ++ (show zs) ++ " = " ++ (show (ys `after` zs))) ys `after` zs

borders :: (Show a, Ord a) => [a] -> [[a]]
borders [] = [[]]
borders xs = xs : borders (border xs)


