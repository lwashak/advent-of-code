module Day01 (Input, Output, parseInput, partOne, partTwo) where

import qualified Common as C (insertIntoSorted)
import Data.Char (isDigit)
import Data.List (sort, transpose)

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
getOccurancesForSorted _ [] = 0
getOccurancesForSorted x (y:ys)
    | x > y  = getOccurancesForSorted x ys
    | x == y = 1 + getOccurancesForSorted x ys
    | x < y  = 0

-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day01.txt"
    let input = parseInput raw
    print $ partOne input
    print $ partTwo input
