type Expression = [Term]
type Term = [Factor] 
type Factor = [Digit] 
type Digit = Int

digits :: Factor
digits = [1 .. 9]

pi_digits :: Factor
pi_digits = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7]

valExpr :: Expression -> Int 
valExpr = sum . map valTerm

valTerm :: Term -> Int
valTerm = product . map valFact 

valFact :: Factor -> Int
valFact = foldl1 (\n d -> 10 * n + d)

expressions :: [Digit] -> [Expression] 
expressions = concatMap partitions . partitions

partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs]
                 ++ [(x:ys):yss | (ys:yss) <- partitions xs]

good :: Int -> Bool
good v = v == 1000

goodOnes :: [Digit] -> [Expression]
goodOnes = filter (good . valExpr) . expressions

main = putStrLn $ "Number of solutions found: " ++ (show $ length $ goodOnes pi_digits)
