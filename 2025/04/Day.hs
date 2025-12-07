module Day where

import           Data.Map     (Map)
import qualified Data.Map     as M
import           Utils.Grid   (zipCoords, getSurroundingCoords8)
import           Utils.Runner (dayRunner)

-- Types
type Grid = Map (Int,Int) Char
type Input = Grid
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = M.fromList . zipCoords . lines

-- Solutions
partOne :: Input -> Output
partOne = snd . removePaper

partTwo :: Input -> Output
partTwo = snd . removeMaxPaper 0

countPaper :: Grid -> Int
countPaper = M.size . M.filter (=='@')

countSurroundingPaper :: Grid -> (Int,Int) -> Int
countSurroundingPaper grid (x,y) = length . filter (\(i,j) -> M.lookup (i,j) grid == Just '@') $ coords 
  where coords = getSurroundingCoords8 (x,y)

canForkliftAccess :: Grid -> (Int,Int) -> Bool
canForkliftAccess grid (x,y) =
  case M.lookup (x,y) grid of
    Just '@' -> countSurroundingPaper grid (x,y) < 4
    _ -> False

removePaper :: Grid -> (Grid, Int)
removePaper grid = (grid', numRemoved)
  where grid' = M.filterWithKey (\k _ -> not $ canForkliftAccess grid k) grid
        numRemoved = countPaper grid - countPaper grid'

removeMaxPaper :: Int -> Grid -> (Grid, Int)
removeMaxPaper n grid
  | numRemoved == 0 = (grid', n)
  | otherwise = removeMaxPaper (n+numRemoved) grid'
  where (grid', numRemoved) = removePaper grid

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo
