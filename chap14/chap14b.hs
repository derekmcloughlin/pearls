import Data.List

maxtail :: Ord a => [a] -> [a]
maxtail = foldl op []

op ys x 
    | ys == []              = [x]
    | head (ys ? zs) >= x   = ys ++ [x ]
    | otherwise             = op zs x
  where zs = border ys
