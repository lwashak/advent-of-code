module Day where

import           Data.List     (nub)
import           Utils.Common  (splitOn)
import           Utils.Numbers (appendInts, countDigits, digitsToInt,
                                intToDigits)
import           Utils.Runner  (dayRunner)

-- Types
type Input = [(Int,Int)]
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = map (parseRange . splitOn '-') . splitOn ','

parseRange :: [String] -> (Int, Int)
parseRange [x,y] = (read x, read y)
parseRange _     = error "Parse Error"

-- Solutions
partOne :: Input -> Output  -- x < 54062341306
partOne = sum . concatMap (generateInvalidIdsForPattern 2)

partTwo :: Input -> Output
partTwo = sum . concatMap generateInvalidIds

-- r is the number of times the pattern can be repeated
generateInvalidIdsForPattern :: Int -> (Int, Int) -> [Int]
generateInvalidIdsForPattern r (x,y)
  | x > y = []
  | r <= 1 = []
  | numDigits `mod` r /= 0 = generateInvalidIdsForPattern r (x+1,y)
  | invalidId > y = []
  | invalidId < x = generateInvalidIdsForPattern r (nextInvalidId,y)
  | otherwise = invalidId : generateInvalidIdsForPattern r (nextInvalidId,y)
  where digits = intToDigits x
        numDigits = length digits
        digitsToRepeat = take (numDigits `div` r) digits
        intToRepeat = digitsToInt digitsToRepeat
        invalidId = digitsToInt $ concat (replicate r digitsToRepeat)
        nextInvalidId = appendInts $ replicate r (intToRepeat+1)

generateInvalidIds :: (Int, Int) -> [Int]
generateInvalidIds (x,y) = nub $ concatMap (\r -> generateInvalidIdsForPattern r (x,y)) [2..countDigits y]


-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo

