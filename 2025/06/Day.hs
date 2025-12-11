module Day where

import Utils.Runner (dayRunner)
import Data.List (transpose)

-- Types
type Problem = (Char, [Int])

type Input = [(Char, [[Char]])]
type Output = Int

-- -- Parsing
parseInput:: String -> Input
parseInput input = zip operators digits
  where blocks = parseBlocks [] . lines $ input
        operators = map (head . last) blocks
        digits = map init blocks

parseBlocks :: [[Char]] -> [String] -> [[[Char]]]
parseBlocks acc ([]:_) = [acc]
parseBlocks acc ls
  | all (==' ') cs = acc : parseBlocks [] ls'
  | otherwise = parseBlocks acc' ls'
  where cs = map head ls
        ls' = map tail ls
        acc' = case acc of
          [] -> map (:[]) cs
          _  -> zipWith (++) acc (map (:[]) cs)

-- Solutions
solveProblem :: Problem -> Int
solveProblem ('+', xs) = sum xs
solveProblem ('*', xs) = product xs
solveProblem _ = error "Unknown operator"

partOne :: Input -> Output
partOne input = sum . map solveProblem $ problems
  where numbers = map (map (read . filter (/=' ')) . snd) input
        problems = zip (map fst input) numbers

partTwo :: Input -> Output
partTwo input = sum . map solveProblem $ problems
  where numbers = map (map (read . filter (/=' ')) . reverse . transpose . snd) input
        problems = zip (map fst input) numbers

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo
