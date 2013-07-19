smallest k ([], ws) = ws !! k
smallest k (zs, []) = zs !! k
smallest k (zs, ws) =
    case (a < b, k <= p + q) of
        (True, True)   -> smallest k (zs, us)
        (True, False)  -> smallest (k - p - 1) (ys, ws)
        (False, True)  -> smallest k (xs, ws)
        (False, False) -> smallest (k - q - 1) (zs, vs)
    where p = (length zs) `div` 2
          q = (length ws) `div` 2
          (xs, a : ys) = splitAt p zs
          (us, b : vs) = splitAt q ws
