module Day02 (part1, part2) where

import qualified Common as C (getFirstInt)

type Report = [Int]

part1 :: String -> Int
part1 input = let rs = map parseReport (lines input)
                  as = map isSafeIncreasing rs
                  bs = map isSafeDecreasing rs
              in sum [if s then 1 else 0 | s <- zipWith (||) as bs]

part2 :: String -> String
part2 _ = "TODO"


parseReport :: String -> Report
parseReport [] = []
parseReport cs = case C.getFirstInt cs of
                      (Just x, cs') -> x : parseReport cs'
                      (Nothing, cs')  -> parseReport cs'


isSafe :: (Int -> Int -> Int) -> Report -> Bool
isSafe _ []  = True
isSafe _ (_:[]) = True
isSafe delta (x1:x2:xs) = let d = delta x1 x2
                              safe = d <= 3 && d > 0
                          in safe && isSafe delta (x2:xs)


isSafeIncreasing :: Report -> Bool
isSafeIncreasing = isSafe (flip (-))

isSafeDecreasing :: Report -> Bool
isSafeDecreasing = isSafe (-)
