
-- A Surpassing Problem

msc xs = maximum [scount z zs | z : zs <- tails xs]

scount x xs = length(filter(x < ) xs)

tails [] = []
tails (x:xs) = (x:xs) : tails xs
