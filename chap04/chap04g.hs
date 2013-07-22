import Data.Array

smallest :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest k (xa, ya) = search k (0, m + 1) (0, n + 1)
    where (0, m) = bounds xa
          (0, n) = bounds ya
          search k (lx, rx) (ly,  ry)
            | lx == rx  = ya!(ly + k)
            | ly == ry  = xa!(lx + k)
            | otherwise = case (xa!mx < ya!my, k <= mx + my - lx - ly) of
                    (True, True)   -> search k (lx, rx) (ly, my)
                    (True, False)  -> search (k - mx - 1 + lx) (mx + 1, rx) (ly, ry)
                    (False, True)  -> search k (lx, mx) (ly, ry)
                    (False, False) -> search (k - my - 1 + ly) (lx, rx) (my + 1, ry)
            where mx = (lx + rx) `div` 2
                  my = (ly + ry) `div` 2
