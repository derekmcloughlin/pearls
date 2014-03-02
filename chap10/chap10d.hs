import System.Environment (getArgs)
import Data.Set

nub' :: Ord a => [a] -> [a]
nub' = hub' empty empty . preprocess

preprocess :: Ord a => [a] -> [(a, Set a)]
preprocess xs = zip xs (tail (scanr insert empty xs))

hub' :: Ord a => Set a -> Set a -> [(a, Set a)] -> [a]
hub' ps ws [] = []
hub' ps ws ((x, xs) : xss) = 
    if member x ps then 
        hub' ps ws xss 
    else case (member x xs, member x ws) of
        (False, False)   -> eus ++ [x] ++ hub' qs empty xss
        (False, True)    -> eus ++ [x] ++ hub' qs vs xss
        (True, False)    -> hub' ps (insert x us) xss
        (True, True)     -> hub' ps ws xss
        where (us, vs) = split x ws
              eus = elems us
              qs = Prelude.foldr insert ps eus

