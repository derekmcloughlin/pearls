import Data.Array

minfree = search . checklist

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n ) (zip (filter (<= n) xs) (repeat True)) 
        where n = length xs

search :: Array Int Bool -> Int 
search = length . takeWhile id . elems
