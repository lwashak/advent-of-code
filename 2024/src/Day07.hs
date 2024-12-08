module Day07 (Input, Output, parseInput, partOne, partTwo, main) where

import Parsing (readInts)

-- Types
type Input = [(Int, [Int])]
type Output = Int

type Op = Int -> Int -> Int

-- Parsing
parseInput :: String -> [(Int, [Int])]
parseInput raw = map parseLine (lines raw)

parseLine :: String -> (Int, [Int])
parseLine s = case readInts s of
    (target : xs) -> (target, xs)
    []            -> error "No Int found in line"

-- Solutions
partOne :: Input -> Output
partOne = sumValidEquations [(*), (+)]

partTwo :: Input -> Output
partTwo = sumValidEquations [(*), (+), concatInts]

sumValidEquations :: [Op] -> [(Int, [Int])] -> Int
sumValidEquations _ [] = 0
sumValidEquations ops ((target, xs):ls) =
    if solve ops target xs
   then target + sumValidEquations ops ls
   else sumValidEquations ops ls

solve :: [Op] -> Int -> [Int] -> Bool
solve _ _ [] = False
solve _ target [x] = x == target
solve ops target (x1:x2:xs)
    | x1 > target = False
    | otherwise   = any (solve ops target) xs' -- Map solve across the different results of (x1 `op` x2), and OR all results together
    where ys  = map (($ x2) . ($ x1)) ops -- Get list of possible values of (y = x1 `op` x2) where `op` is the different operators
          xs' = map (:xs) ys              -- Construct the list of all (y:xs)

concatInts :: Int -> Int -> Int
concatInts x y = read $ show x ++ show y

-- -- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day07.txt"
    let input = parseInput raw
    print input
    print $ partOne input
    print $ partTwo input
