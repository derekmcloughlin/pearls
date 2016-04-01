import Data.Char
import Data.List
import Data.List.Ordered
import Data.List.Split

type Cell = Int
type Grid = [(Cell, Cell)] 
type Vehicle = Char
type Move = (Vehicle, Cell) 
type State = Grid
type Path = ([Move], State) 
type Frontier = [Path]

blankGrid :: String
blankGrid = replicate 36 '.'

g1 :: Grid
g1 = [  (17, 18), -- Special vehicle - i.e. our car.
        (1, 15), 
        (2, 9), 
        (3, 10), 
        (4, 11), 
        (5, 6), 
        (12, 19), 
        (13, 27), 
        (24, 26), 
        (31, 38), 
        (33, 34), 
        (36, 37), 
        (40, 41)]

g2 :: Grid
g2 = [  (16, 17), -- Special vehicle - i.e. our car.
        (1, 2),
        (8, 22),
        (29, 36),
        (11, 25),
        (38, 40),
        (33, 34),
        (6, 20)]

drawGrid :: Grid -> IO ()
drawGrid g = putStr $ intersperse ' ' $ "\n" ++ unlines (chunksOf 6 $ showGrid g)

cellToArrayPos :: Cell -> Int
cellToArrayPos c = (c `div` 7) * 6 + (c `mod` 7) - 1

vehiclePositions :: Grid -> [[Cell]]
vehiclePositions  = map vehiclePos
  where
    vehiclePos (a, b) 
        | isHorizontal (a, b)   = map cellToArrayPos  [a .. b]
        | otherwise             = map cellToArrayPos $ verticalPositions (a, b)

verticalPositions :: (Cell, Cell) -> [Int]
verticalPositions (a, b) 
    | b - a > 12    = [a, a + 7, b]
    | b - a == 7    = [a, b]
    | otherwise     = []

showVehicle :: (Char, [Int]) -> String
showVehicle (c, xs) = gridPoints xs 0
  where
    gridPoints [] n   = replicate (36 - n) '.'
    gridPoints (x:xs) n = replicate (x - n) '.' ++ [c] ++ gridPoints xs (x + 1)

superimpose :: String -> String -> String
superimpose = zipWith combine
  where
    combine :: Char -> Char -> Char
    combine topCh bottomCh
        | topCh == '.' && bottomCh == '.'   = '.'
        | topCh == '.' && bottomCh /= '.'   = bottomCh
        | topCh /= '.' && bottomCh == '.'   = topCh
        | topCh /= '.' && bottomCh /= '.'   = 'X'

showGrid :: Grid -> String
showGrid g = foldl superimpose blankGrid vehicleGrids
  where
    vehicles = zip vehicleNames (vehiclePositions g)
    vehicleGrids = map showVehicle vehicles

vehicleNames :: String
vehicleNames = "@" ++ ['a'..] -- The first vehicle is our special one - we mark it with an @.

-- allocateNames :: Grid -> NamedGrid
-- allocateNames g = zipWith (\cell ch -> (fst cell, snd cell, ch)) g vehicleNames

isVertical :: (Cell, Cell) -> Bool
isVertical (c1, c2) = c2 - c1 > 6 

isHorizontal :: (Cell, Cell) -> Bool
isHorizontal (c1, c2) = c2 - c1 < 6 

occupied :: Grid -> [Cell]
occupied = foldr (merge . fillcells) []

fillcells :: (Enum a, Num a, Ord a) => (a, a) -> [a]
fillcells (r, f) = if r > f - 7 then [r .. f] else [r, r + 7 .. f]

freecells :: Grid -> [Cell] 
freecells g = allcells \\ occupied g

allcells :: [Cell]
allcells = [c | c <- [1 .. 41], c `mod` 7 /= 0]

moves :: Grid -> [Move]
moves g = [(v, c) | (v, i) <- zip vehicleNames g, c <- adjs i, c `elem` fs]
  where 
    fs :: [Cell]
    fs = freecells g
    adjs :: (Num a, Ord a) => (a, a) -> [a]
    adjs (r, f) = if r > f - 7 then [f + 1, r - 1] else [f + 7,r - 7]

move :: Grid -> Move -> Grid
move g (v, c) = g1 ++ adjust i c:g2
  where 
    (g1, i:g2)  = splitAt v1 g
    v1          = if v == '@' then 0 else ord v - ord 'a' + 1

adjust :: (Num a, Ord a) => (a, a) -> a -> (a, a)
adjust (r, f ) c
    | r > f - 7 = if c > f then (r + 1, c) else (c, f - 1) 
    | otherwise = if c < r then (c, f - 7) else (r + 7, c)

solved :: Grid -> Bool 
solved g = snd (head g) == 20

bfsolve :: Grid -> Maybe [Move]
bfsolve g = bfsearch [] [([], g)]

bfsearch :: [State] -> Frontier -> Maybe [Move] 
bfsearch qs [] = Nothing
bfsearch qs (p@(ms, q) : ps)
    | solved q      = Just ms
    | q `elem` qs   = bfsearch qs ps
    | otherwise     = bfsearch (q:qs) (ps ++ succs p)

succs :: Path -> [Path]
succs (ms, q) = [(ms ++ [m], move q m) | m <- moves q]

bfsolve' :: Grid -> Maybe [Move]
bfsolve' g = bfsearch' [] [] [([], g)]

bfsearch' :: [State] -> [Frontier] -> Frontier -> Maybe [Move] 
bfsearch' qs [] [] = Nothing
bfsearch' qs pss [ ] = bfsearch' qs [] (concat (reverse pss))
bfsearch' qs pss (p@(ms, q) : ps)
    | solved q      = Just ms
    | q `elem` qs   = bfsearch' qs pss ps
    | otherwise     = bfsearch' (q:qs) (succs p:pss) ps


dfsolve :: Grid -> Maybe [Move]
dfsolve g = dfsearch [] [([], g)]

dfsearch :: [State] -> Frontier -> Maybe [Move] 
dfsearch qs [] = Nothing
dfsearch qs (p@(ms, q) : ps)
    | solved q      = Just ms
    | q `elem` qs   = dfsearch qs ps
    | otherwise     = dfsearch (q:qs) (succs p ++ps)


-- Worse Case Scenarios
hardest :: Grid
hardest = [ (17, 18), -- Special vehicle - i.e. our car.
        (1, 3), 
        (4, 11), 
        (5, 19), 
        (6, 20), 
        (8, 15), 
        (9, 10), 
        (22, 23), 
        (24, 31), 
        (30, 37), 
        (33, 34), 
        (38, 39), 
        (40, 41)]


