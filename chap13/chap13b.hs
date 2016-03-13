import Data.List

transform :: Ord a => [a] -> ([a], Int)
transform xs = (map last xss, position xs xss)
  where 
    xss = sort (rots xs)

position :: Eq a => a -> [a] -> Int
position xs xss = length (takeWhile (/= xs) xss)

rots :: [a] -> [[a]]
rots xs = take (length xs) (iterate lrot xs)
  where
    lrot :: [a] -> [a]
    lrot [] = []
    lrot (y:ys) = ys ++ [y]

takeCols :: Int -> [[a]] -> [[a]]
takeCols j = map (take j)

recreate :: Ord a => Int -> [a] -> [[a]]
recreate 0 = map (const [])
recreate j = hdsort . consCol . fork (id, recreate (j - 1))


hdsort :: Ord a => [[a]] -> [[a]]
hdsort = sortBy cmp
  where 
    cmp (x:xs) (y:ys) = compare x y

consCol :: ([a], [[a]]) -> [[a]]
consCol (xs, xss) = zipWith (:) xs xss

fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f, g) x = (f x, g x)

-- Note: in the book, it doesn't mention that the first argument to the first 
-- call of `recreate` needs to be the length of the transformed string.
untransform :: Ord a => ([a], Int) -> [a]
untransform (ys, k) = (recreate (length ys) ys) !! k 

main :: IO ()
main = do
    hContents <- readFile "test.txt"
    let result = untransform $ transform hContents
    putStrLn $ if result == hContents then
        "matched"
    else
        "NOT MATCHED"
