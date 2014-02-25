data Tree = Leaf Int |
            Fork Tree Tree
            deriving (Show, Eq)

tree_1 :: Tree
tree_1 = Fork (Leaf 3) (Leaf 5)

tree_2 :: Tree
tree_2 = Fork 
            (Fork 
                (Fork (Leaf 8) (Leaf 2))
                (Leaf 7))
            (Fork 
                (Leaf 9)
                (Fork (Leaf 6) 
                    (Fork (Leaf 3) (Leaf 5))))

cost :: Tree -> Int
cost (Leaf x) = x
cost (Fork u v) = 1 + max (cost u) (cost v)

fringe :: Tree -> [Int]
fringe (Leaf x) = [x]
fringe (Fork u v) = fringe u ++ fringe v

foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b 
foldrn f g [x]      = g x
foldrn f g (x:xs)   = f x (foldrn f g xs)

type Forest = [Tree]

trees :: [Int] -> [Tree]
trees = map rollup . forests

forests :: [Int ] -> [Forest]
forests = foldrn (concatMap . prefixes) (wrap . wrap . Leaf )

wrap :: a -> [a]
wrap x = [x]

prefixes :: Int -> Forest -> [Forest ] 
prefixes x ts = [Leaf x : rollup (take k ts) : drop k ts | k <- [1 .. length ts]]

rollup :: Forest -> Tree 
rollup = foldl1 Fork

minBy :: Ord b => (a -> b) -> [a] -> a
minBy f = foldl1 (cmp f)

cmp :: Ord b => (a -> b) -> a -> a -> a
cmp f u v = if f u <= f v then u else v

mincostTree :: [Int] -> Tree
mincostTree = foldl1 Fork . map snd . foldrn insert (wrap . leaf)

insert :: Int -> [(Int, Tree)] -> [(Int, Tree)]
insert x ts = leaf x : split x ts

split :: Int -> [(Int, Tree)] -> [(Int, Tree)]
split x [u] = [u]
split x (u : v : ts) = if x `max` fst u < fst v then u : v : ts
                       else split x (fork u v : ts)

leaf :: Int -> (Int, Tree)
leaf x = (x , Leaf x )

fork :: (Int, Tree) -> (Int, Tree) -> (Int, Tree)
fork (a, u) (b, v) = (1 + a `max` b, Fork u v )


