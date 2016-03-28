import Data.List
import Data.List.Split

type Cell = Int
type Grid = [(Cell, Cell)] 

blankGrid :: String
blankGrid = replicate 36 '.'

-- The grid from the book
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

-- The grid from the image
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

showGrid :: Grid -> String
showGrid g = foldl superimpose blankGrid vehicleGrids
  where
    vehicles = zip vehicleNames (vehiclePositions g)
    vehicleGrids = map showVehicle vehicles

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

vehicleNames :: String
vehicleNames = "@" ++ ['a'..] -- The first vehicle is our special one - we mark it with an @.

isVertical :: (Cell, Cell) -> Bool
isVertical (c1, c2) = c2 - c1 > 6 

isHorizontal :: (Cell, Cell) -> Bool
isHorizontal (c1, c2) = c2 - c1 < 6 
