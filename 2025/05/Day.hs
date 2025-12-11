module Day where

import Utils.Runner (dayRunner)
import Utils.Parsing (readInts)

-- Types
type Input = ([(Int,Int)], [Int])
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = parseIngredientIds [] [] . lines

parseIngredientIds :: [(Int,Int)] -> [Int] -> [String] -> Input
parseIngredientIds rangeAcc idAcc [] = (rangeAcc, idAcc)
parseIngredientIds rangeAcc idAcc (l:ls) = case readInts l of
  [x,y] -> parseIngredientIds ((x,y):rangeAcc) idAcc ls
  [x] -> parseIngredientIds rangeAcc (x:idAcc) ls
  _ -> parseIngredientIds rangeAcc idAcc ls

-- Solutions
partOne :: Input -> Output
partOne (db,ids) = length . filter (isIngredientFresh db) $ ids

isIngredientFresh :: [(Int,Int)] -> Int -> Bool
isIngredientFresh [] _ = False
isIngredientFresh ((x,y):db) i
  | i >= x && i <= y = True
  | otherwise = isIngredientFresh db i

partTwo :: Input -> Output
partTwo = getAllPossibleFreshIngredients . filter (uncurry (<=)) . removeOverlappingRanges . fst

removeOverlappingRanges :: [(Int,Int)] -> [(Int,Int)]
removeOverlappingRanges [] = []
removeOverlappingRanges ((x,y):db) = getNonOverlappingRanges db (x,y) ++ removeOverlappingRanges db

getNonOverlappingRanges :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
getNonOverlappingRanges [] (a,b) = [(a,b)]
getNonOverlappingRanges ((x,y):db) (a,b)
  | a > y || b < x = getNonOverlappingRanges db (a,b)                                         -- Not overlapping
  | a > x && b < y = []                                                                     -- (a,b) lies inside range (x,y)
  | a < x && b > y = getNonOverlappingRanges db (a,x-1) ++ getNonOverlappingRanges db (y+1,b) -- (x,y) lies inside range (a,b)
  | b >= x && b <= y = getNonOverlappingRanges db (a,x-1)
  | a <= y && a >= x = getNonOverlappingRanges db (y+1,b)
  | otherwise = error "Unhandled range case"

getAllPossibleFreshIngredients :: [(Int,Int)] -> Int
getAllPossibleFreshIngredients [] = 0
getAllPossibleFreshIngredients ((x,y):ids) = y - x + 1 + getAllPossibleFreshIngredients ids

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo
