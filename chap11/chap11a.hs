mnss :: [Int] -> Int
mnss = maximum . map sum . nonsegs

testData :: [Int]
testData = [-4, -3, -7, 2, 1, -2, -1, -4]

markings :: [a] -> [[(a, Bool)]]
markings xs = [zip xs bs | bs <- booleans (length xs)]

booleans 0 = [[]]
booleans n = [b : bs | b <- [True, False], bs <- booleans (n - 1)]

nonsegs :: [a] -> [[a]]
nonsegs = extract . filter nonseg . markings

extract :: [[(a, Bool)]] -> [[a]]
extract = map (map fst . filter snd)

data State = E | S | M | N
    deriving(Eq, Show)

nonseg :: [(a, Bool)] -> Bool
nonseg = (== N) . foldl step E . map snd

step :: State -> Bool -> State
step E False = E 
step E True = S 
step S False = M 
step S True = S 
step M False = M
step M True = N
step N False = N
step N True = N

