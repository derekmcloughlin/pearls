type Expression = [Term]
type Term = [Factor] 
type Factor = [Digit] 
type Digit = Int

digits :: Factor
digits = [1 .. 9]

valExpr :: Expression -> Int 
valExpr = sum . map valTerm

valTerm :: Term -> Int
valTerm = product . map valFact 

valFact :: Factor -> Int
valFact = foldl1 (\n d -> 10 * n + d)

expressions :: [Digit] -> [Expression] 
expressions = foldr extend [ ]

extend :: Digit -> [Expression] -> [Expression] 
extend x [] = [[[[x]]]]
extend x es = concatMap (glue x) es

glue :: Digit -> Expression -> [Expression] 
glue x ((xs:xss):xsss) = [((x:xs):xss):xsss,
                          ([x] : xs : xss) : xsss, 
                          [[x]] : (xs : xss) : xsss]

good :: Int -> Bool
good v = v == 100

goodOnes :: [Digit] -> [Expression]
goodOnes = filter (good . valExpr) . expressions

