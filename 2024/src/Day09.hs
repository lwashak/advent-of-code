module Day09 (Input, Output, parseInput, partOne, partTwo, main) where

import Parsing (readDigits)

import Data.Maybe (isJust, fromJust, isNothing)

-- Types
type Input = [Block]
type Output = Int

type Block = (Maybe Int, Int)

-- Parsing
parseInput :: String -> Input
parseInput = filter (/=(Nothing, 0)) . getBlocks 0 . readDigits

getBlocks :: Int -> [Int] -> [Block]
getBlocks _ [] = []
getBlocks i [n] = [(Just i, n)]
getBlocks i (n1:n2:ns) = (Just i, n1) : (Nothing, n2) : getBlocks (i + 1) ns

-- Solutions
partOne :: Input -> Int
partOne = computeChecksum 0 . compressBlocks

partTwo :: Input -> Output
partTwo = computeChecksum 0 . compressFiles

compressBlocks :: [Block] -> [Block]
compressBlocks bs = moveBlocks bs (reverse $ filter (isJust . fst) bs)

moveBlocks :: [Block] -> [Block] -> [Block]
moveBlocks ((Just i, n) : xs) ys = (Just i, n) : moveBlocks xs ys
moveBlocks ((Nothing, free) : xs) ((Just i, n) : ys)
    | nextFile >= i = [(Just i, n)]
    | free == n = (Just i, n) : moveBlocks xs ys
    | free < n  = (Just i, free) : moveBlocks xs ((Just i, (n - free)): ys)
    | free > n  = (Just i, n) : moveBlocks ((Nothing, (free - n)) : xs) ys
    where nextFile = fromJust $ fst $ head $ dropWhile (isNothing . fst) xs
moveBlocks _ _ = error "Unreachable"

compressFiles :: [Block] -> [Block]
compressFiles bs = moveFiles bs (reverse $ filter (isJust . fst) bs)

moveFiles :: [Block] -> [Block] -> [Block]
moveFiles xs [] = xs
moveFiles xs ((Just i, n) : ys) = moveFiles xs' ys
    where xs' = moveFile xs (Just i, n)
moveFiles _ _ = error "Can't move non-file"

moveFile :: [Block] -> Block -> [Block]
moveFile ((Just i, n) : xs) b@(Just j, _)
    | j == i    = (Just i, n) : xs
    | otherwise = (Just i, n) : moveFile xs b
moveFile ((Nothing, free) : xs) b@(Just i, n)
    | free < n  = (Nothing, free) : moveFile xs b
    | free == n = (Just i, n) : deleteFile xs i
    | free > n  = (Just i, n) : (Nothing, (free - n)) : deleteFile xs i
moveFile [] _ = []
moveFile _ _ = error "Can't move non-file"

deleteFile :: [Block] -> Int -> [Block]
deleteFile [] _ = []
deleteFile ((Nothing, n) : xs) fileId = (Nothing, n) : deleteFile xs fileId
deleteFile ((Just i, n) : xs) fileId
    | i == fileId = (Nothing, n) : xs
    | otherwise   = (Just i, n) : deleteFile xs fileId

computeChecksum :: Int -> [(Maybe Int, Int)] -> Int
computeChecksum _ [] = 0
computeChecksum pos ((Nothing, n): xs) = computeChecksum (pos + n) xs
computeChecksum pos ((Just i, n): xs) = checksum + computeChecksum (pos + n) xs
    where checksum = sum $ zipWith (*) (replicate n i) [pos..]

-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day09.txt"
    let input = parseInput raw
    print input
    print $ partOne input
    print $ partTwo input
