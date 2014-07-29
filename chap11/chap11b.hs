testData :: [Int]
testData = [-4, -3, -7, 2, 1, -2, -1, -4]

testDataBig :: [Int]
testDataBig = [-4, -3, -7, 2, 1, -2, -1, -4, 5, 7, -8, 3, 2, -1, -4, 5, -9, 3]

mnss :: [Int] -> Int
mnss xs = fourth (foldl h (start (take 3 xs)) (drop 3 xs))

h :: (Num a,  Ord a) => (a, a, a, a) -> a -> (a, a, a, a)
h (e, s, m, n) x = (e, max s e + x, max m s, max n (max n m + x))

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

start :: (Num a,  Ord a) => [a] -> (a, a, a, a)
start xs = (0, maximum [x + y + z, y + z, z], maximum [x, x + y, y], x + z)
    where x = xs!!0
          y = xs!!1
          z = xs!!2
