module Utils.Parsing (readInts, readDigits) where

import           Data.Char (digitToInt, isDigit)

readInts :: String -> [Int]
readInts "" = []
readInts s@(c:_)
    | isDigit c = let (x, cs) = span isDigit s
                  in read x : readInts cs
    | otherwise = readInts $ dropWhile (not . isDigit) s

readDigits :: String -> [Int]
readDigits "" = []
readDigits (c:cs)
    | isDigit c = digitToInt c : readDigits cs
    | otherwise = readDigits cs

