module Day11 (Input, Output, parseInput, partOne, partTwo, main) where

-- import qualified Control.Parallel.Strategies as P

import Parsing (readInts)
-- import Common (applyN)

import Data.List (group, sort, splitAt)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

-- Types
type Input = IntMap Int
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput raw = let xss = group $ sort $ readInts raw
                     occurances = zip (map head xss) (map length xss)
                 in M.fromList occurances

-- Solutions
partOne :: Input -> Output
partOne = sum . M.elems . (!! 25) . iterate blink

partTwo :: Input -> Output
partTwo = sum . M.elems . (!! 75) . iterate blink

blink :: IntMap Int -> IntMap Int
blink stones = fst $ M.mapAccumWithKey go M.empty stones
    where go stones' stone count = case mutateStone stone of
                                        [s]      -> (M.alter (addStone count) s stones', stones)
                                        [s1, s2] -> (M.alter (addStone count) s1 $ M.alter (addStone count) s2 stones', stones)
                                        _ -> error "Expecting no more than 2 stones"

addStone :: Int -> Maybe Int -> Maybe Int
addStone count' Nothing     = Just count'
addStone count' (Just count)  = Just (count + count')

mutateStone :: Int -> [Int]
mutateStone x
    | x == 0               = [1]
    | even (length digits) = splitStones digits
    | otherwise            = [x * 2024]
    where digits = intToDigits x

intToDigits :: Int -> [Int]
intToDigits x
    | x < 0 = intToDigits (-1 * x)
    | x >= 0 && x <= 9 = [x]
    | x >= 10 = let d1 = x `div` 10
                    d2 = x - (d1 * 10)
                in intToDigits d1 ++ [d2]
intToDigits _ = error "Impossible"

digitsToInt :: [Int] -> Int
digitsToInt = digitsToInt' 0

digitsToInt' :: Int -> [Int] -> Int
digitsToInt' acc [] = acc
digitsToInt' acc (x:xs) = digitsToInt' acc' xs
    where acc' = (acc * 10) + x

splitStones :: [Int] -> [Int]
splitStones xs = map digitsToInt [l, r]
    where (l,r) = splitAt (length xs `div` 2) xs

-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day11.txt"
    let input = parseInput raw
    print input
    print $ partOne input
    print $ partTwo input
