import Data.Array
import Data.Array.ST

minfree = search . checklist

checklist :: [Int] -> Array Int Bool
checklist xs = runSTArray (do
        {a <- newArray (0, n) False;
        sequence [writeArray a x True | x <- xs, x <= n];
        return a}
    )
    where n = length xs

search :: Array Int Bool -> Int 
search = length . takeWhile id . elems
