module Day01 (part1, part2) where

import qualified Common as C (getFirstInt, insertIntoSorted)
import Data.Char (isDigit)


part1 :: String -> Int
part1 input =
    let ls = lines input
        (xs, ys) = parseLines ls [] []
    in sum (getDistances xs ys)


part2 :: String -> Int
part2 input =
    let ls = lines input
        (xs, ys) = parseLines ls [] []
    in getSimilarityScore xs ys


parseLines :: [String] -> [Int] -> [Int] -> ([Int], [Int])
parseLines [] xs ys = (xs, ys)
parseLines (l:ls) xs ys = case C.getFirstInt l of
    (Nothing, _) -> error "Line should contain 2 numbers"
    (Just x, l') -> case C.getFirstInt l' of
        (Nothing, _) -> error "Line should contain 2 numbers"
        (Just y, _) -> parseLines ls (C.insertIntoSorted x xs) (C.insertIntoSorted y ys)


getDistances :: [Int] -> [Int] -> [Int]
getDistances = zipWith (\x y -> abs (x - y))


getSimilarityScore :: [Int] -> [Int] -> Int
getSimilarityScore [] _ = 0
getSimilarityScore (x:xs) ys =
    let n = getOccurancesForSorted x ys
    in (x * n) + getSimilarityScore xs ys


getOccurancesForSorted :: Int -> [Int] -> Int
getOccurancesForSorted _ [] = 0
getOccurancesForSorted x (y:ys)
    | x > y  = getOccurancesForSorted x ys
    | x == y = 1 + getOccurancesForSorted x ys
    | x < y  = 0
