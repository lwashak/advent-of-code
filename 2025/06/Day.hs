module Day where

import Utils.Runner (dayRunner)
import Data.List (transpose)
import Utils.Numbers (intToDigits, digitsToInt)
import Debug.Trace

-- Types
type Problem = ([Int], Char)

type Input = ([[Int]], [Char])
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput input =
  let wordsInLines = map words . lines $ input
      numbers = transpose . map (map read) . init $ wordsInLines
      operators = map head . last $ wordsInLines
  in (numbers, operators)

-- Solutions
solveProblem :: Problem -> Int
solveProblem (xs, '+') = sum xs
solveProblem (xs, '*') = product xs
solveProblem _ = error "Unknown operator"

getColumns :: [Int] -> [Int]
getColumns = map digitsToInt . reverse . transpose . map intToDigits

partOne :: Input -> Output
partOne (xss, ops) = sum $ zipWith (curry solveProblem) xss ops

partTwo :: Input -> Output
partTwo (xss, ops) = sum $ zipWith (curry solveProblem) (trace (show numbers) numbers) ops
  where numbers = map getColumns xss  -- TODO - bugged because need to account for alignment in columns :(

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo
