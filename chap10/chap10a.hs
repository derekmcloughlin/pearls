import System.Environment (getArgs)

notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = notElem n a

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

nub' :: Ord a => [a] -> [a]
nub' [] = []
nub' (x:xs) = if notElem x xs then x : nub' xs 
              else (x : nub' (xs \\ [x])) `min` (nub' xs)

main = do
    args <- getArgs
    let word = case args of
                    [] -> "calculus"
                    (x:_) -> x
    putStrLn $ show $ nub' word
