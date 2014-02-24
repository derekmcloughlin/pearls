type Expression = [Term]
type Term = [Factor] 
type Factor = [Digit] 
type Digit = Int

digits :: Factor
digits = [1 .. 9]

pi_digits :: Factor
pi_digits = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7]

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) (x, y) = (f x, g y)

good c (_, f, t, e)  = (f * t + e ==c)
ok c (_, f, t, e)    = (f * t + e <= c)

modify x (k, f, t, e) = [(10 * k, k * x + f, t, e), (10, x, f * t, e), (10, x, 1, f * t + e)]

solutions :: Int -> [Digit] -> [Expression]
solutions c = map fst . filter (good c . snd) . foldr (expand c) []

expand c x [] = [([[[x]]], (10, x, 1, 0))]
expand c x evs = concat (map (filter (ok c . snd) . glue x) evs)

glue x ((xs : xss) : xsss, (k, f, t, e)) =
    [(((x : xs) : xss) : xsss, (10*k, k*x + f, t, e)),
    (([x] : xs : xss) : xsss, (10, x, f * t, e)),
    ([[x]] : (xs : xss) : xsss, (10, x, 1, f * t + e))]

main = putStrLn $ "Number of solutions found: " ++ (show $ length $ solutions 1000 pi_digits)
