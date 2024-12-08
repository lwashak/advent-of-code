module Parsing (readInts) where

import Data.Char (isDigit)

readInts :: String -> [Int]
readInts "" = []
readInts s@(c:_)
    | isDigit c = let (x, cs) = span isDigit s
                  in read x : readInts cs
    | otherwise = readInts $ dropWhile (not . isDigit) s
