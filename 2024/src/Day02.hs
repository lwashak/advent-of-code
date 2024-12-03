module Day02 (Input, Output, parseInput, partOne, partTwo) where

import qualified Common as C (getFirstInt)

type Report = [Int]
type Input = [Report]
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = map (map read . words) . lines


-- Solutions
partOne :: [Report] -> Int
partOne rs = let as = map (isSafe safeIncrease) rs
                 bs = map (isSafe safeDecrease) rs
             in sum [if s then 1 else 0 | s <- zipWith (||) as bs]

partTwo :: [Report] -> Int
partTwo rs = let as = map (isSafeWithDampener safeIncrease) rs
                 bs = map (isSafeWithDampener safeDecrease) rs
             in sum [if s then 1 else 0 | s <- zipWith (||) as bs]

isSafe :: (Int -> Int -> Bool) -> Report -> Bool
isSafe _ []  = True
isSafe _ [_] = True
isSafe safe (x1:x2:xs) = safe x1 x2 && isSafe safe (x2:xs)

safeIncrease :: Int -> Int -> Bool
safeIncrease x y = safeDelta (y - x)

safeDecrease :: Int -> Int -> Bool
safeDecrease x y = safeDelta (x - y)

safeDelta :: Int -> Bool
safeDelta d = d <= 3 && d > 0

isSafeWithDampener :: (Int -> Int -> Bool) -> Report -> Bool
isSafeWithDampener = isSafeWithDampener' False Nothing

isSafeWithDampener' :: Bool -> Maybe Int -> (Int -> Int -> Bool) -> Report -> Bool
isSafeWithDampener' _ _ _ []  = True
isSafeWithDampener' _ _ _ (_:[]) = True
isSafeWithDampener' prevFailure mx0 safe (x1:x2:xs)
    | safe x1 x2  = isSafeWithDampener' prevFailure (Just x1) safe (x2:xs)
    | prevFailure = False
    | otherwise   = case mx0 of
                        Just x0 -> isSafeWithDampener' True Nothing safe (x0:x2:xs) || isSafeWithDampener' True Nothing safe (x0:x1:xs)
                        Nothing -> isSafeWithDampener' True Nothing safe (x2:xs)    || isSafeWithDampener' True Nothing safe (x1:xs)
