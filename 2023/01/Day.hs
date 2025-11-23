module Day where

import           Data.Char     (digitToInt, isDigit)
import           Data.List     (isPrefixOf)
import           Data.Maybe    (mapMaybe)
import           Utils.Parsing (readDigits)
import           Utils.Runner  (dayRunner)

-- Input
type Input = String
type Output = Int

-- Parsing
parseInput :: (String -> [Int]) -> String -> [(Int, Int)]
parseInput f = mapMaybe (getFirstAndLast . f) . lines

readDigitsAndWords :: String -> [Int]
readDigitsAndWords "" = []
readDigitsAndWords cs@(c:cs')
    | isDigit c = digitToInt c : readDigitsAndWords cs'
    | "one" `isPrefixOf` cs = 1 : readDigitsAndWords cs'
    | "two" `isPrefixOf` cs = 2 : readDigitsAndWords cs'
    | "three" `isPrefixOf` cs = 3 : readDigitsAndWords cs'
    | "four" `isPrefixOf` cs = 4 : readDigitsAndWords cs'
    | "five" `isPrefixOf` cs = 5 : readDigitsAndWords cs'
    | "six" `isPrefixOf` cs = 6 : readDigitsAndWords cs'
    | "seven" `isPrefixOf` cs = 7 : readDigitsAndWords cs'
    | "eight" `isPrefixOf` cs = 8 : readDigitsAndWords cs'
    | "nine" `isPrefixOf` cs = 9 : readDigitsAndWords cs'
    | otherwise = readDigitsAndWords cs'

getFirstAndLast :: [Int] -> Maybe (Int, Int)
getFirstAndLast []       = Nothing
getFirstAndLast [x]      = Just (x, x)
getFirstAndLast (x : xs) = Just (x, last xs)

-- Solutions
partOne :: Input -> Output
partOne input = sum [x * 10 + y | (x, y) <- pairs]
  where pairs = parseInput readDigits input

partTwo :: Input -> Output
partTwo input = sum [x * 10 + y | (x, y) <- pairs]
  where pairs = parseInput readDigitsAndWords input


-- Main
main :: IO [()]
main = dayRunner id partOne partTwo

