notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = notElem n a

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

nub' [] = []
nub' (x:xs) = if notElem x xs then x : nub' xs 
              else (x : nub' (xs \\ [x])) `min` (nub' xs)
