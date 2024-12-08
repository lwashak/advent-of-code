module Day08 (main) where

import Common (findIndices2D)

import Data.Char (isLower, isUpper, isDigit)
import Data.List (nub)


--Types
type Point = (Int, Int)
type Bounds = (Int, Int)

type Input = ([[Point]], Bounds)
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput raw = let grid = lines raw
                     lowers = findIndices2D isLower grid
                     uppers = findIndices2D isUpper grid
                     digits = findIndices2D isDigit grid
                     height = length grid
                     width  = length $ head grid
                 in ([lowers, uppers, digits], (width, height))

-- Solutions
partOne :: Input -> Int
partOne (pss, bounds) = length $ nub $ foldMap (getAntinodes bounds) pss

getAntinodes :: (Int, Int) -> [Point] -> [Point]
getAntinodes b as = let antennaPairs = [ (p1,p2) | p1 <- as, p2 <- as, p1 < p2 ]
                        antennaMirrors = foldMap (uncurry getMirrorPoints) antennaPairs
                        antinodes = filter (\p -> not (isOutOfBounds b p) && p `notElem` as) antennaMirrors
                    in antinodes

getMirrorPoints :: Point -> Point -> [Point]
getMirrorPoints (i1,j1) (i2,j2) =
    let d1 = (i2 - i1, j2 - j1)
        m1 = (i1 - fst d1, j1 - snd d1)
        m2 = (i2 + fst d1, j2 + snd d1)
    in [m1, m2]

isOutOfBounds :: (Int, Int) -> Point -> Bool
isOutOfBounds (w,h) (i,j) = i < 0 || j < 0 || i >= w || j >= h

partTwo :: Input -> Output
partTwo _ = 2



-- Main
main :: IO ()
main = do
    raw <- readFile "../input/example/Day08.txt"
    let input = parseInput raw
    print input
    print $ partOne input  -- 2315 (high), 2290 (high)
    print $ partTwo input
