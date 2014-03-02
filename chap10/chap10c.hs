import System.Environment (getArgs)
import Data.Set

nub' :: Ord a => [a] -> [a]
nub' = hub empty . preprocess

preprocess :: Ord a => [a] -> [(a, Set a)]
preprocess xs = zip xs (tail (scanr insert empty xs))

hub :: Ord a => Set a -> [(a, Set a)] -> [a]
hub ws [] = []
hub ws ((x,xs) : xss) = case (member x xs, member x ws) of
    (False,False)   -> eus ++ [x] ++ hub empty yss
    (False,True)    -> eus ++ [x] ++ hub vs yss
    (True,False)    -> hub (insert x us) xss
    (True,True)     -> hub ws xss
    where (us,vs) = split x ws
          eus = elems us
          yss = [(x, xs) | (x, xs) <- xss, not (member x us)]

main = do
    args <- getArgs
    let word = case args of
                    [] -> "calculus"
                    (x:_) -> x
    putStrLn $ show $ nub' word

