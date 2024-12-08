module Day08 (Input, Output, parseInput, partOne, partTwo, main) where

import Common (findIndices2D)

import Data.List (nub)


--Types
type Point = (Int, Int)
type Bounds = (Int, Int)

type Input = ([[Point]], Bounds)
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput raw = let grid = lines raw
                     chars = nub $ concatMap (filter (/= '.')) grid
                     antennas = map (\c -> findIndices2D (==c) grid) chars
                     height = length grid
                     width  = length $ head grid
                 in (antennas, (width, height))

-- Solutions
partOne :: Input -> Output
partOne (pss, bounds) = length $ nub $ foldMap (getAntinodes (getMirrorPoints bounds)) pss

partTwo :: Input -> Output
partTwo (pss, bounds) = length $ nub $ foldMap (getAntinodes (getResonantPoints bounds)) pss

getAntinodes :: (Point -> Point -> [Point]) -> [Point] -> [Point]
getAntinodes f as = let antennaPairs = [ (p1,p2) | p1 <- as, p2 <- as, p1 < p2 ]
                        antinodes = foldMap (uncurry f) antennaPairs
                    in antinodes

getMirrorPoints :: (Int, Int) -> Point -> Point -> [Point]
getMirrorPoints b p1 p2 =
    let d  = p1 `subtractPoint` p2
        m1 = p1 `addPoint` d
        m2 = p2 `subtractPoint` d
    in filter (isInBounds b) [m1, m2]

getResonantPoints :: (Int, Int) -> Point -> Point -> [Point]
getResonantPoints b p1 p2 =
    let d   = p1 `subtractPoint` p2
        ds  = map (`multiplyPoint` d) [0..]
        rs1 = takeWhile (isInBounds b) $ map (addPoint p1) ds
        rs2 = takeWhile (isInBounds b) $ map (subtractPoint p2) ds
    in rs1 ++ rs2

addPoint :: Point -> Point -> Point
addPoint (i1,j1) (i2,j2) = (i1 + i2, j1 + j2)

subtractPoint :: Point -> Point -> Point
subtractPoint (i1,j1) (i2,j2) = (i1 - i2, j1 - j2)

multiplyPoint :: Int -> Point -> Point
multiplyPoint n (i,j) = (i * n, j * n)

isInBounds :: (Int, Int) -> Point -> Bool
isInBounds (w,h) (i,j) = i >= 0 && j >= 0 && i < w && j < h


-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day08.txt"
    let input = parseInput raw
    print input
    print $ partOne input
    print $ partTwo input
