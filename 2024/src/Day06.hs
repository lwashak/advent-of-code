module Day06 () where

import Data.List (nub, find)
import Data.Either (fromRight)
import Data.Maybe (isJust)

-- Types
type Pos = (Int, Int)
type Bounds = (Int, Int)

type Path = [(Pos, Direction)]

data Direction = N | E | S | W
    deriving (Show, Read, Eq)

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
partOne g = case walk [] g of
    Right vs -> length $ nub (map fst vs)
    Left vs  -> error "Loop found"

walk :: [(Pos, Direction)] -> Grid -> Either Path Path
walk visited Grid{ obstacles=os, playerPos=p, playerDirection=d, size=s }
    | loop               = Left visited
    | isOutOfBounds s p' = Right visited'
    | p' `elem` os       = walk visited Grid { obstacles=os, playerPos=p, playerDirection=turn d, size=s }
    | otherwise          = walk visited' Grid { obstacles=os, playerPos=p', playerDirection=d, size=s }
    where loop = (p, d) `elem` visited
          p'   = move d p
          visited' = visited ++ [(p,d)]

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

partTwo :: Grid -> Int
partTwo g@Grid{ obstacles=os, playerPos=p } =
    let path  = tail $ fromRight [] $ walk [] g
        os'   = filter (/=p) $ getPotentialLoopObstacles os path
        loops = filter (not . tryWalkWithObstacle g) (nub os')
    in length loops

tryWalkWithObstacle :: Grid -> Pos -> Bool
tryWalkWithObstacle Grid{ obstacles=os, playerPos=p, playerDirection=d, size=s } o =
    case walk [] Grid{ obstacles=(o:os), playerPos=p, playerDirection=d, size=s } of
         Right _ -> True -- True if no loop
         Left _  -> False

getPotentialLoopObstacles :: [Pos] -> Path -> [Pos]
getPotentialLoopObstacles _ [] = []
getPotentialLoopObstacles _ [p] = []
getPotentialLoopObstacles os ((p0,d0):(p1,d1):path)
    | isJust futureObstacle = p1 : getPotentialLoopObstacles os ((p1,d1):path)
    | otherwise = getPotentialLoopObstacles os ((p1,d1):path)
    where futureObstacle = findInDirection (turn d0) p0 os

findInDirection :: Direction -> Pos -> [Pos] -> Maybe Pos
findInDirection N (x,y) = find (\(x',y') -> x' == x && y' < y)
findInDirection E (x,y) = find (\(x',y') -> x' > x && y' == y)
findInDirection S (x,y) = find (\(x',y') -> x' == x && y' > y)
findInDirection W (x,y) = find (\(x',y') -> x' < x && y' == y)



-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day06.txt"
    let input = parseInput raw
--     print input
    print $ partOne input
    print $ partTwo input
