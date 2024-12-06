module Day06 () where

import Data.List (nub)

-- Types
type Pos = (Int, Int)
type Bounds = (Int, Int)

data Direction = N | E | S | W
    deriving (Show, Read)

data Grid = Grid { size :: Bounds
                 , obstacles :: [Pos]
                 , playerPos :: Pos
                 , playerDirection :: Direction
                 } deriving (Show, Read)

-- Parsing
parseInput :: String -> Grid
parseInput raw = let css = lines raw
                     os  = findObstacles css
                     p   = findPlayer css
                     s   = (length $ head css, length css)
                 in Grid { size=s, obstacles=os, playerPos=p, playerDirection=N }

findCharInGrid :: Char -> Pos -> [[Char]] -> [Pos]
findCharInGrid _ _ [] = []
findCharInGrid t (x,y) ([]:css) = findCharInGrid t (0,y+1) css
findCharInGrid t (x,y) ((c:cs):css)
    | c == t    = (x,y) : findCharInGrid t (x+1, y) (cs:css)
    | otherwise = findCharInGrid t (x+1, y) (cs:css)

findObstacles :: [[Char]] -> [Pos]
findObstacles = findCharInGrid '#' (0,0)

findPlayer :: [[Char]] -> Pos
findPlayer = head . findCharInGrid '^' (0,0)

-- Solutions
partOne :: Grid -> Int
partOne = length . nub . walk

walk :: Grid -> [Pos]
walk Grid{ obstacles=os, playerPos=p, playerDirection=d, size=s }
    | isOutOfBounds s p' = [p]
    | p' `elem` os       = walk Grid { obstacles=os, playerPos=p, playerDirection=turn d, size=s }
    | otherwise          = p : walk Grid { obstacles=os, playerPos=p', playerDirection=d, size=s }
    where p' = move d p

isOutOfBounds :: Bounds -> Pos -> Bool
isOutOfBounds (l,h) (x,y)
    | x > l || x < 0 = True
    | y > h || y < 0 = True
    | otherwise      = False

move :: Direction -> Pos -> Pos
move N (x, y) = (x, y-1)
move E (x, y) = (x+1, y)
move S (x, y) = (x, y+1)
move W (x, y) = (x-1, y)

turn :: Direction -> Direction
turn N = E
turn E = S
turn S = W
turn W = N

findPotentialLoops :: [Pos] -> [[Pos]]
findPotentialLoops [] = []
findPotentialLoops ((x,y):ps) = let xs = filter (\(x',y') -> x' == x) ps
                                    ys = filter (\(x',y') -> y' == y) ps


-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day06.txt"
    let input = parseInput raw
--     print input
    print $ partOne input
--     print $ partTwo input
