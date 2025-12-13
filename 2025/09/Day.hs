module Day where

import           Data.List    (maximumBy, sortBy)
import           Data.Ord     (comparing)
import           Utils.Runner (dayRunner)

-- Types
type Pos = (Int,Int)
type Input = [Pos]
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = map (\l -> read $ "(" ++ l ++ ")"). lines

-- Solutions
area :: Pos -> Pos -> Int
area (x1,y1) (x2,y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

largestArea :: [Pos] -> (Pos,Pos)
largestArea ps = maximumBy (comparing $ uncurry area) ([(a,b) | a <- ps, b <- ps, a < b])

partOne :: Input -> Output
partOne = uncurry area . largestArea

partTwo :: Input -> Output
partTwo = error "Not Implemented"

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo
