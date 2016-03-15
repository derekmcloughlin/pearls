module Queue
where

data Queue a = Q !Int [a] !Int [a]
               deriving Show

empty :: Queue a
empty = Q 0 [] 0 []

head :: Queue a -> a
head (Q lf (f:fs) lb bs) = f
head (Q _ [] _ _) = error "empty queue"

tail :: Queue a -> Queue a
tail (Q lf (f:fs) lb bs) = makeQ (lf - 1) fs lb bs
tail (Q _ [] _ _) = error "empty queue"

snoc :: a -> Queue a -> Queue a
snoc x (Q lf fs lb bs) = makeQ lf fs (lb + 1) (x:bs)

makeQ :: Int -> [a] -> Int -> [a] -> Queue a
makeQ lf fs lb bs
    | lf > lb       = Q lf fs lb bs
    | otherwise     = Q (lf + lb) (fs ++ reverse bs) 0 []

elemsQueue :: Queue a -> [a]
elemsQueue (Q lf fs lb bs) = fs ++ (reverse bs)

insert = flip snoc

remove :: Queue a -> (a, Queue a)
remove (Q lf (f:fs) lb bs) = (f, makeQ (lf - 1) fs lb bs)
remove (Q _ [] _ _) = error "empty queue"

