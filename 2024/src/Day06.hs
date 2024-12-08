module Day06 (Input, Output, parseInput, partOne, partTwo, main) where

import Common (findIndices2D)

import Data.List (nub)
import Data.Maybe (isNothing, fromJust)

import Data.Set (Set)
import qualified Data.Set as S

-- Types
type Input = World
type Output = Int

type Pos = (Int, Int)
type Path = [Pos]

data Direction = N | E | S | W
    deriving (Show, Read, Eq, Ord)

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

findObstacles :: [[Char]] -> [Pos]
findObstacles = findIndices2D (=='#')

findGuard :: [[Char]] -> Pos
findGuard = head . findIndices2D (=='^')


-- Solutions
partOne :: Input -> Output
partOne w = case walkPatrol S.empty [] w of
    Just path -> length $ nub (guardPos w : path)
    Nothing   -> error "Loop detected"

partTwo :: Input -> Output
partTwo w =
    let path = nub $ fromJust $ walkPatrol S.empty [] w
    in findLoops w path

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
              lastPos = last ((i,j) : path)

walkPatrol :: Set (Pos, Direction) -> Path -> World -> Maybe Path
walkPatrol visited path w = case walkStraight w of
    Left (p,_) -> Just (p ++ path)
    Right (p, (i,j)) -> let newDir = turn $ guardDir w
                            visited' = ((i,j), guardDir w) `S.insert` visited
                            loop = S.size visited == S.size visited'
                        in if loop then Nothing
                                   else walkPatrol visited' (p ++ path) (World (obstacles w) (i,j) newDir (width w) (height w))

turn :: Direction -> Direction
turn N = E
turn E = S
turn S = W
turn W = N

isLoop :: World -> Bool
isLoop = isNothing . walkPatrol S.empty []

findLoops :: World -> Path -> Int
findLoops _ [] = 0
findLoops w (p:path)
    | isLoop w' = 1 + findLoops w path
    | otherwise = findLoops w path
    where os' = S.insert p (obstacles w)
          w' = w { obstacles=os' }

-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day06.txt"
    let input = parseInput raw
    print input
    print $ partOne input
    print $ partTwo input

