module Day11 (Input, Output, parseInput, partOne, partTwo, main) where

import Parsing (readInts)
import Common (applyN)

-- Types
type Input = [Int]
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = readInts

-- Solutions
partOne :: Input -> Output
partOne xs = length $ applyN 25 blink xs

partTwo :: Input -> Output
partTwo xs = length $ applyN 75 blink xs

blink :: [Int] -> [Int]
blink [] = []
blink (x:xs)
    | x == 0               = 1 : blink xs
    | even (length digits) = map digitsToInt (splitList digits) ++ blink xs
    | otherwise            = x * 2024 : blink xs
    where digits = intToDigits x

intToDigits :: Int -> [Int]
intToDigits x
    | x < 0 = intToDigits (-1 * x)
    | x >= 0 && x <= 9 = [x]
    | x >= 10 = let d1 = x `div` 10
                    d2 = x - (d1 * 10)
                in intToDigits d1 ++ [d2]
    | otherwise = error "Impossible"

digitsToInt :: [Int] -> Int
digitsToInt = digitsToInt' 0

digitsToInt' :: Int -> [Int] -> Int
digitsToInt' acc [] = acc
digitsToInt' acc (x:xs) = digitsToInt' acc' xs
    where acc' = (acc * 10) + x

splitList :: [a] -> [[a]]
splitList xs = let mid = (length xs) `div` 2
                   as  = take mid xs
                   bs  = drop mid xs
               in [as, bs]

-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day11.txt"
    let input = parseInput raw
    print input
    print $ partOne input
    print $ partTwo input
