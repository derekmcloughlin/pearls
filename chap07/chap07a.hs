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

tree_3 :: Tree
tree_3 = Fork 
            (Fork 
                (Fork 
                    (Leaf 1) 
                    (Leaf 2)) 
                (Leaf 3)) 
            (Fork 
                (Leaf 6) 
                (Fork 
                    (Leaf 5) (Leaf 4)))

tree_4 :: Tree
tree_4 = Fork 
            (Fork 
                (Fork 
                    (Leaf 1) 
                    (Fork 
                        (Leaf 2) 
                        (Fork 
                            (Leaf 3) 
                            (Leaf 6)))) 
                (Leaf 5)) 
            (Leaf 4)


fringe :: Tree -> [Int]
fringe (Leaf x) = [x]
fringe (Fork u v) = fringe u ++ fringe v

cost :: Tree -> Int
cost (Leaf x) = x
cost (Fork u v) = 1 + max (cost u) (cost v)

trees :: [Int] -> [Tree]
trees [x] = [Leaf x]
trees (x:xs) = concatMap (prefixes x) (trees xs)

prefixes :: Int -> Tree -> [Tree] 
prefixes x t@(Leaf y)   = [Fork (Leaf x) t]
prefixes x t@(Fork u v) = [Fork (Leaf x) t] ++
                          [Fork u' v | u' <- prefixes x u]


