module Day02 (part1, part2) where

import qualified Common as C (getFirstInt)

type Report = [Int]

part1 :: String -> Int
part1 input = let rs = map parseReport (lines input)
                  as = map (isSafe safeIncrease) rs
                  bs = map (isSafe safeDecrease) rs
              in sum [if s then 1 else 0 | s <- zipWith (||) as bs]


part2 :: String -> Int
part2 input = let rs = map parseReport (lines input)
                  as = map (isSafeWithDampener safeIncrease) rs
                  bs = map (isSafeWithDampener safeDecrease) rs
              in sum [if s then 1 else 0 | s <- zipWith (||) as bs]


parseReport :: String -> Report
parseReport [] = []
parseReport cs = case C.getFirstInt cs of
                      (Just x, cs') -> x : parseReport cs'
                      (Nothing, cs')  -> parseReport cs'


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
