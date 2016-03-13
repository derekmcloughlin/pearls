import Data.List 

transform :: Ord a => [a] -> ([a], Int) 
transform xs = (map last xss, position xs xss)
  where 
    xss = sort (rots xs)

position :: Eq a => a -> [a] -> Int
position xs xss = length (takeWhile (/= xs) xss)

rots :: [a] -> [[a]]
rots xs = take (length xs) (iterate lrot xs)
  where
    lrot :: [a] -> [a]
    lrot [] = []
    lrot (y:ys) = ys ++ [y]


