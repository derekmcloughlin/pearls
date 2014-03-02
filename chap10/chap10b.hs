import System.Environment (getArgs)

notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = notElem n a

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

nub' :: Ord a => [a] -> [a]
nub' [] = []
nub' (x:xs) = if notElem x xs then x : nub' xs 
              else (x : nub' (xs \\ [x])) `min` (nub' xs)

nub'' :: Ord a => [a] -> [a]
nub'' = hub []

hub :: Ord a => [a] -> [a] -> [a]
hub ws []       = []
hub ws (x:xs)   = case (x `elem` xs, x `elem` ws) of
                    (False, False)  -> us ++ [x] ++ hub [] (xs\\us) 
                    (False,True)    -> us ++ [x] ++ hub (tail vs) (xs\\us) 
                    (True, False)   -> hub (us ++ [x]) xs
                    (True, True)    -> hub ws xs
                  where (us, vs) = span (< x) ws

main = do
    args <- getArgs
    let word = case args of
                    [] -> "calculus"
                    (x:_) -> x
    putStrLn $ show $ nub'' word


