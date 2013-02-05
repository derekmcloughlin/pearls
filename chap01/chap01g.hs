-- The Smallest Free Number

-- Array-based Solution

minfree :: [Integer] -> Integer
minfree xs = minfrom 0 xs

minfrom :: Integer -> [Integer] -> Integer
minfrom a xs = head ([a ..] \\ xs)


(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = notElem n a

