module Day06 () where

import Data.List (nub, find)
import Data.Either (fromRight)
import Data.Maybe (isJust)

import Data.Set (Set)
import qualified Data.Set as S (map, member, notMember, fromList, size, insert)

-- Types
type Pos = (Int, Int)

type Path = [Pos]

data Direction = N | E | S | W
    deriving (Show, Read, Eq)

data World = World { obstacles :: Set Pos
                   , guardPos :: Pos
                   , guardDir :: Direction
                   , width :: Int
                   , height :: Int
                   } deriving (Show, Read)

-- Parsing
parseInput :: String -> World
parseInput raw = World obstacles guardPos guardDir width height
    where grid      = lines raw
          obstacles = S.fromList $ findObstacles grid
          guardPos  = findGuard grid
          guardDir  = N
          width     = length $ head grid
          height    = length grid

findCharInGrid :: Pos -> Char -> [[Char]] -> [Pos]
findCharInGrid _ _ [] = []
findCharInGrid (i,j) t ([]:css) = findCharInGrid (0,j+1) t css
findCharInGrid (i,j) t ((c:cs):css)
    | c == t    = (i,j) : findCharInGrid (i+1, j) t (cs:css)
    | otherwise = findCharInGrid (i+1, j) t (cs:css)

findObstacles :: [[Char]] -> [Pos]
findObstacles = findCharInGrid (0,0) '#'

findGuard :: [[Char]] -> Pos
findGuard = head . findCharInGrid (0,0) '^'


-- Solutions
partOne :: World -> Int
partOne w = length $ nub (guardPos w : walkPatrol w)

isOnEdge :: (Int, Int) -> Pos -> Bool
isOnEdge (w, h) (i,j) = i >= w - 1 || i <= 0 || j >= h - 1 || j <= 0

walkStraight :: World -> Either (Path, Pos) (Path, Pos)
walkStraight w =
    if isOnEdge (width w, height w) lastPos
    then Left (path, lastPos) -- If path walks off the map, return Left path
    else Right (path, lastPos)
        where (i, j) = guardPos w
              obs    = obstacles w
              line | guardDir w == N = zip (repeat i) (reverse [0 .. j-1])
                   | guardDir w == E = zip [i+1 .. width w -1] (repeat j)
                   | guardDir w == S = zip (repeat i) [j+1 .. height w -1]
                   | otherwise       = zip (reverse [0 .. i-1]) (repeat j)
              path    = takeWhile (`S.notMember` obs) line
              lastPos = last path

walkPatrol :: World -> Path
walkPatrol w = case walkStraight w of
    Left (p,_) -> p
    Right (p, (i,j)) -> p ++ walkPatrol (World (obstacles w) (i,j) newDir (width w) (height w))
    where newDir = turn $ guardDir w

turn :: Direction -> Direction
turn N = E
turn E = S
turn S = W
turn W = N

-- walk :: Set (Pos, Direction) -> World -> Either Path Path
-- walk visited World{ obstacles=os, playerPos=p, playerDirection=d, size=s }
--     | loop               = Left visited
--     | isOutOfBounds s p' = Right visited'
--     | p' `S.member` os   = walk visited World { obstacles=os, playerPos=p, playerDirection=turn d, size=s }
--     | otherwise          = walk visited' World { obstacles=os, playerPos=p', playerDirection=d, size=s }
--     where loop = (p, d) `S.member` visited
--           p'   = move d p
--           visited' = visited ++ [(p,d)]
--
--
-- move :: Direction -> Pos -> Pos
-- move N (x, y) = (x, y-1)
-- move E (x, y) = (x+1, y)
-- move S (x, y) = (x, y+1)
-- move W (x, y) = (x-1, y)



-- partTwo :: World -> Int
-- partTwo g =
--     let path  = fromRight [] $ walk [] g
--     in findLoops g path
--
-- isLoop :: World -> Bool
-- isLoop g = case walk [] g of
--     Left _  -> True
--     Right _ -> False
--
-- findLoops :: World -> Path -> Int
-- findLoops _ [] = 0
-- findLoops _ [p] = 0
-- findLoops g@World{ obstacles=os, size=s } ((p0,d0):(p1,d1):path)
--     | isNextObstacle && isLoop g' = 1 + findLoops g ((p1,d1):path)
--     | otherwise = findLoops g ((p1,d1):path)
--     where isNextObstacle = isJust $ findInDirection (turn d0) p0 os
--           os' = S.insert p1 os
--           g' = World{ obstacles=os', playerPos=p0, playerDirection=d0, size=s}
--
-- findInDirection :: Direction -> Pos -> Set Pos -> Maybe Pos
-- findInDirection N (x,y) = find (\(x',y') -> x' == x && y' < y)
-- findInDirection E (x,y) = find (\(x',y') -> x' > x && y' == y)
-- findInDirection S (x,y) = find (\(x',y') -> x' == x && y' > y)
-- findInDirection W (x,y) = find (\(x',y') -> x' < x && y' == y)



-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day06.txt"
    let input = parseInput raw
    print input
    print $ partOne input
--     print $ partTwo input
