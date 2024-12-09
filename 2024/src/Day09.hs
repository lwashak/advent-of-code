module Day09 (Input, Output, parseInput, partOne, partTwo, main) where

import Parsing (readDigits)

-- Types
type Input = [Int]
type Output = Int

-- Parsing
parseInput :: String -> [Int]
parseInput = getFragmentedBlocks 0 [] . readDigits

getFragmentedBlocks :: Int -> [Int] -> [Int] -> [Int]
getFragmentedBlocks _ blocks [] = blocks
getFragmentedBlocks blockId blocks (x1:x2:xs) = getFragmentedBlocks (blockId + 1) (blocks ++ newBlocks) xs
    where dataBlocks = replicate x1 blockId
          freeBlocks = replicate x2 (-1)
          newBlocks  = dataBlocks ++ freeBlocks
getFragmentedBlocks blockId blocks [x] = blocks ++ dataBlocks
    where dataBlocks = replicate x blockId

-- Solutions
partOne :: Input -> Int
partOne blocks = computeChecksum $ defragmentBlocks blocks (reverse $ filter (>= 0) blocks)

defragmentBlocks :: [Int] -> [Int] -> [Int]
defragmentBlocks _ [] = []
defragmentBlocks [] _ = []
defragmentBlocks (x:xs) (y:ys)
    | x >= 0 = x : defragmentBlocks xs (init (y:ys))
    | otherwise = y : defragmentBlocks xs ys

computeChecksum :: [Int] -> Int
computeChecksum xs = sum $ zipWith (*) xs [0..]

partTwo :: Input -> Output
partTwo _ = 2

-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day09.txt"
    let input = parseInput raw
--     print input
    print $ partOne input
    print $ partTwo input
