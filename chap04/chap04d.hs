import Debug.Trace
import Text.Printf

smallest k ([], ws) = trace ("[], " ++ show ws) ws!!k
smallest k (zs, []) = trace (show zs ++ ", []") zs!!k
smallest k (zs, ws) =
    case (a < b, k <= p + q) of
        (True, True)   -> trace ("TT" ++ dump_vars k zs ws p q a b us vs) smallest k (zs, us)
        (True, False)  -> trace ("TF" ++ dump_vars k zs ws p q a b us vs) smallest (k - p - 1) (ys, ws)
        (False, True)  -> trace ("FT" ++ dump_vars k zs ws p q a b us vs) smallest k (xs, ws)
        (False, False) -> trace ("FF" ++ dump_vars k zs ws p q a b us vs) smallest (k - q - 1) (zs, vs)
    where p = (length zs) `div` 2
          q = (length ws) `div` 2
          (xs, a : ys) = splitAt p zs
          (us, b : vs) = splitAt q ws

dump_vars k zs ws p q a b us vs =
    " k = " ++ (printf "%2d" k :: String) ++ " " ++
    "zs = " ++ (printf "%15s" (show zs) :: String) ++ " " ++
    "ws = " ++ (printf "%15s" (show ws) :: String) ++ " " ++
    " p = " ++ (printf "%2d" p :: String) ++ " " ++
    " q = " ++ (printf "%2d" q :: String) ++ " " ++
    " a = " ++ (printf "%2d" a :: String) ++ " " ++
    " b = " ++ (printf "%2d" b :: String) ++ " " ++
    "us = " ++ (printf "%7s" (show us) :: String) ++ " " ++
    "vs = " ++ (printf "%7s" (show vs) :: String) ++ " " ++
    "kd = " ++ (printf "%2d" (p + q) :: String) 

xs = [1, 2, 3, 9]
ys = [4, 10, 13, 33, 67]

