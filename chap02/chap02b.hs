-- A surpassing problem

scount x xs = length(filter(x < ) xs)

tails [] = []
tails (x:xs) = (x:xs) : tails xs

table xs = [(z, scount z zs) | z : zs <- tails xs]

msc xs = maximum . map snd $ table xs
