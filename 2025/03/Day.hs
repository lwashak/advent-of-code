module Day where

import Utils.Runner (dayRunner)
import Utils.Parsing (readDigits)

-- Types
type Input = [[Int]]
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = map readDigits . lines

-- Solutions
partOne :: Input -> Output
partOne = sum . map (findMaxJoltage 0 2)

partTwo :: Input -> Output
partTwo = sum . map (findMaxJoltage 0 12)

findMaxJoltage :: Int -> Int -> [Int] -> Int
findMaxJoltage acc 0 _ = acc
findMaxJoltage acc n xs = findMaxJoltage acc' (n-1) xs' 
  where toSearch = take (length xs - (n-1)) xs
        x        = foldr max 0 toSearch
        xs'      = tail $ dropWhile (/=x) xs
        acc'     = acc * 10 + x

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo
