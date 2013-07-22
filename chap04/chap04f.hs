import Data.Array
import Debug.Trace

smallest :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest k (xa, ya) = search k (0, m + 1) (0, n + 1)
    where (0, m) = bounds xa
          (0, n) = bounds ya
          search k (lx, rx) (ly,  ry)
          --                              changes here:                           vvvvvv
            | lx == rx  = trace ("lx == rx" ++ dump_vars k lx rx ly ry mx my) ya!(ly + k)
          --                              changes here:                           vvvvvv
            | ly == ry  = trace ("ly == ry" ++ dump_vars k lx rx ly ry mx my) xa!(lx + k)
          --                              changes here:     vvvvvvvvv
            | otherwise = case (xa!mx < ya!my, k <= mx + my - lx - ly) of
                    (True, True)   -> trace ("TT: " ++ dump_vars k lx rx ly ry mx my) search k (lx, rx) (ly, my)
          --                              changes here:                                                  vvvv   vvvvvv
                    (True, False)  -> trace ("TF: " ++ dump_vars k lx rx ly ry mx my) search (k - mx - 1 + lx) (mx + 1, rx) (ly, ry)
                    (False, True)  -> trace ("FT: " ++ dump_vars k lx rx ly ry mx my) search k (lx, mx) (ly, ry)
          --                              changes here:                                                  vvvv   vvvvvv
                    (False, False) -> trace ("FF: " ++ dump_vars k lx rx ly ry mx my) search (k - my - 1 + ly) (lx, rx) (my + 1, ry)
            where mx = (lx + rx) `div` 2
                  my = (ly + ry) `div` 2


dump_vars k lx rx ly ry mx my =
    " k = " ++ show k ++ "\t" ++
    "lx = " ++ show lx ++ "\t" ++
    "rx = " ++ show rx ++ "\t" ++
    "ly = " ++ show ly ++ "\t" ++
    "ry = " ++ show ry ++ "\t" ++
    "a = " ++ (if (0, mx) <= bounds xa then show(xa!mx) else "-") ++ "\t" ++
    "b = " ++ (if (0, my) <= bounds ya then show(ya!my) else "-") ++ "\t" ++
    "mx = " ++ show mx ++ "\t" ++
    "my = " ++ show my ++ "\t" ++
    "kd = " ++ show (mx + my - lx - ly)

xs = [1, 2, 3, 9]
ys = [4, 10, 13, 33, 67]

xa = listArray(0, length xs - 1) xs
ya = listArray(0, length ys - 1) ys


