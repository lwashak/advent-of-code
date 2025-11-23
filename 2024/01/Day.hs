module Day where

import           Utils.Runner (dayRunner)

import qualified Utils.Common as C (insertIntoSorted)

-- Types
type Input = ([Int], [Int])
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = parseLines [] [] . lines

parseLines :: [Int] -> [Int] -> [String] -> Input
parseLines xs ys []     = (xs, ys)
parseLines xs ys (l:ls) = let (x,y) = parseLine l
                          in parseLines (C.insertIntoSorted x xs) (C.insertIntoSorted y ys) ls

parseLine :: String -> (Int, Int)
parseLine = toPair . map read . words
    where toPair [a, b] = (a, b)
          toPair _      = error "Expecting 2 numbers per line"

-- Solutions
partOne :: Input -> Output
partOne (xs, ys) = sum (getDistances xs ys)
    where getDistances = zipWith (\x y -> abs (x - y))

partTwo :: Input -> Output
partTwo (xs, ys) = getSimilarityScore xs ys

getSimilarityScore :: [Int] -> [Int] -> Int
getSimilarityScore [] _ = 0
getSimilarityScore (x:xs) ys =
    let n = getOccurancesForSorted x ys
    in (x * n) + getSimilarityScore xs ys

getOccurancesForSorted :: Int -> [Int] -> Int
getOccurancesForSorted x (y:ys)
    | x > y  = getOccurancesForSorted x ys
    | x == y = 1 + getOccurancesForSorted x ys
    | x < y  = 0
getOccurancesForSorted _ _ = 0


-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo

