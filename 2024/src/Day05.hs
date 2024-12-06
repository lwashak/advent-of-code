module Day05 () where

import Common (splitOn)

import Data.List (sortBy)

import qualified Text.Parsec as P
import Text.Parsec (Parsec, (<|>))

-- Types
type Input = (Rules, [[Int]])
type Output = Int

type Rules = [(Int,Int)]

-- Parsing
parseInput :: String -> Input
parseInput raw = case P.parse inputParser "" raw of
    Right i -> i
    Left e  -> error (show e)

inputParser :: Parsec String () Input
inputParser = do
    rs <- P.many (ruleParser <* P.endOfLine)
    _ <- P.endOfLine
    ps <- P.many (pagesParser <* P.endOfLine)
    return (rs, ps)

numberParser :: Parsec String () Int
numberParser = do
    x <- P.many1 P.digit
    return (read x)

ruleParser :: Parsec String () (Int, Int)
ruleParser = do
    x <- numberParser
    _ <- P.char '|'
    y <- numberParser
    return (x, y)

pagesParser :: Parsec String () [Int]
pagesParser = P.many $ do
    x <- numberParser
    _ <- P.optionMaybe $ P.char ','
    return x

-- Solutions
partOne :: Input -> Output
partOne (rs, ps) =
    let ordered = filter (isPageListOrdered rs) ps
    in sum [ xs !! (length xs `div` 2) | xs <- ordered]

partTwo :: Input -> Output
partTwo (rs, ps) =
    let unordered = filter (not . isPageListOrdered rs) ps
        reordered = map (sortBy (getPageOrdering rs)) unordered
    in sum [ xs !! (length xs `div` 2) | xs <- reordered]


isPageListOrdered :: Rules -> [Int] -> Bool
isPageListOrdered rs (x1:x2:xs)
    | order == GT = isPageListOrdered rs (x2:xs)
    | otherwise   = False
    where order = getPageOrdering rs x1 x2
isPageListOrdered _ _ = True

getPageOrdering :: Rules -> Int -> Int -> Ordering
getPageOrdering rs x y
    | (x,y) `elem` rs = GT
    | (y,x) `elem` rs = LT
    | otherwise       = EQ


-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day05.txt"
    let input = parseInput raw
--     print input
    print $ partOne input
    print $ partTwo input
