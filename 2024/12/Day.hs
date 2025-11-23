module Day where

import           Utils.Runner (dayRunner)

import           Data.Array.IArray

import           Data.Set          (Set)
import qualified Data.Set          as Set

-- Types
type Input = Array (Int, Int) Char
type Output = Int

type Grid = Array (Int, Int) Char

-- Parsing
parseInput :: String -> Input
parseInput raw =
    let css = lines raw
        rows = length css
        cols = length (head css)
    in listArray ((0,0),(rows-1, cols-1)) (concat css)


-- Solutions
partOne :: Input -> Output
partOne grid = let positions = [(i, j) | ((i, j), _) <- assocs grid]
                   fences = getPlots Set.empty grid positions
               in sum $ [ area * perim | (area, perim) <- fences ]

getPlots :: Set (Int, Int) -> Grid -> [(Int, Int)] -> [(Int, Int)]
getPlots _ _ [] = []
getPlots visited grid ((i,j):ps)
    | (i,j) `Set.member` visited = getPlots visited grid ps
    | otherwise = let (tiles, perim) = bfs 0 grid Set.empty [(i,j)]
                      area = Set.size tiles
                      visited' = Set.union visited tiles
                  in (area, perim) : getPlots visited' grid ps

partTwo :: Input -> Output
partTwo _ = 2

bfs :: Int -> Grid -> Set (Int, Int) -> [(Int, Int)] -> (Set (Int, Int), Int)
bfs perim _ visited [] = (visited, perim)
bfs perim grid visited ((i, j): xs)
    | (i,j) `Set.member` visited = bfs perim grid visited xs
    | otherwise                  = bfs perim' grid visited' xs'
    where neighbours = getNeighbours grid (i, j)
          perim'     = perim + 4 - length neighbours
          xs'        = xs ++ filter (`Set.notMember` visited) neighbours
          visited'   = (i, j) `Set.insert` visited

getNeighbours :: Grid -> (Int, Int) -> [(Int, Int)]
getNeighbours grid (i, j) = filter isValidNeighbour [(i+1,j),(i,j+1),(i-1,j),(i,j-1)]
    where plant = grid ! (i, j)
          isValidNeighbour (x, y) = isInBounds (bounds grid) (x, y) && (grid ! (x, y) == plant)

isInBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
isInBounds ((rowsMin, colsMin), (rowsMax, colsMax)) (i, j) = i <= rowsMax && i >= rowsMin && j <= colsMax && j >= colsMin


-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo

