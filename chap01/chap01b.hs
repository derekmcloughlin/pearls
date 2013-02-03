-- The Smallest Free Number

-- Array-based Solution using accumArray



minfree :: [Integer] -> Integer
minfree xs = head([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = notElem n a

