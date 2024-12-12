module Day10 (Input, Output, parseInput, partOne, partTwo, main) where

import Data.Array.IArray

import Data.Char (digitToInt)
import Data.List (nub)

-- Types
type Input = Grid
type Output = Int

type Grid = Array (Int, Int) Int

-- Parsing
parseInput :: String -> Input
parseInput raw = listArray ((0, 0), (rows - 1, cols - 1)) (concat xss)
    where xss  = map (map digitToInt) (lines raw)
          rows = length xss
          cols = length (head xss)

-- Solutions
partOne :: Input -> Output
partOne grid = sum $ map (length . nub . dfs grid . pure) (getTrailheads grid)

partTwo :: Input -> Output
partTwo grid = sum $ map (length . dfs grid . pure) (getTrailheads grid)

getTrailheads :: Grid -> [(Int, Int)]
getTrailheads grid = [ (i, j) | ((i, j), h) <- assocs grid, h == 0 ]

dfs :: Grid -> [(Int, Int)] -> [(Int, Int)]
dfs _ [] = []
dfs grid ((i, j): xs)
    | height == 9    = (i, j) : dfs grid  xs
    | otherwise      = dfs grid xs'
    where height     = grid ! (i, j)
          neighbours = getNeighbours grid (i, j)
          xs'        = neighbours ++ xs

getNeighbours :: Grid -> (Int, Int) -> [(Int, Int)]
getNeighbours grid (i, j) = filter isValidNeighbour [(i+1,j),(i,j+1),(i-1,j),(i,j-1)]
    where height = grid ! (i, j)
          isValidNeighbour (x, y) = isInBounds (bounds grid) (x, y) && (grid ! (x, y) == height + 1)

isInBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
isInBounds ((rowsMin, colsMin), (rowsMax, colsMax)) (i, j) = i <= rowsMax && i >= rowsMin && j <= colsMax && j >= colsMin

-- Main
main :: IO ()
main = do
    raw <- readFile "../input/example/Day10.txt"
    let input = parseInput raw
    print input
    print $ partOne input
    print $ partTwo input
