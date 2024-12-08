module Day04 (Input, Output, parseInput, partOne, partTwo, main) where

import Data.List (transpose)

-- Types
type Input = Matrix
type Output = Int

type Matrix = [[Char]]

-- Parsing
parseInput :: String -> Input
parseInput = lines

-- Solutions
partOne :: Input -> Output
partOne m = let rs = getRows m
                cs = getColumns m
                ds1 = getDiagonals m
                ds2 = getDiagonals (rotate m)
                toSearch = rs ++ cs ++ ds1 ++ ds2
            in sum (map countXmas toSearch)

countXmas :: String -> Int
countXmas "" = 0
countXmas ('X':'M':'A':'S':cs) = 1 + countXmas ('S':cs)
countXmas ('S':'A':'M':'X':cs) = 1 + countXmas ('X':cs)
countXmas (_:cs) = countXmas cs

getColumns :: Matrix -> [String]
getColumns = transpose

getRows :: Matrix -> [String]
getRows = id

getDiagonals :: Matrix -> [String]
getDiagonals m =
    let upperDiagonals = getUpperDiagonals (tail m)
        lowerDiagonals = getUpperDiagonals (tail $ transpose m)
    in upperDiagonals ++ [getDiagonal m] ++ lowerDiagonals

getUpperDiagonals :: Matrix -> [String]
getUpperDiagonals [] = []
getUpperDiagonals ([]:_) = []
getUpperDiagonals (r:rs) = getDiagonal (r:rs) : getUpperDiagonals rs

getDiagonal :: Matrix -> String
getDiagonal [] = []
getDiagonal ([]:_) = []
getDiagonal ((c:_):rs) = c : getDiagonal (map tail rs)

rotate :: Matrix -> Matrix
rotate = reverse . transpose

partTwo :: Input -> Output
partTwo = findCrossMases

findCrossMases :: Matrix -> Int
findCrossMases (r1:r2:r3:rs) = findCrossMasesInRows [r1,r2,r3] + findCrossMases (r2:r3:rs)
findCrossMases _ = 0

findCrossMasesInRows :: Matrix -> Int
findCrossMasesInRows ([]:_) = 0
findCrossMasesInRows m@[_,_,_]
    | isCrossMas m = 1 + findCrossMasesInRows (map tail m)
    | otherwise    = findCrossMasesInRows (map tail m)
findCrossMasesInRows _ = error "Expecting 3 rows"

isCrossMas :: Matrix -> Bool
isCrossMas [] = False
isCrossMas ([]:_) = False
isCrossMas ((c11:_:c13:_):(_:c22:_:_):(c31:_:c33:_):_)
    | isMas [c11,c22,c33] && isMas [c31,c22,c13] = True
    | otherwise = False
    where isMas s = s == "MAS" || reverse s == "MAS"
isCrossMas _ = False


-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day04.txt"
    let input = parseInput raw
    print input
    print $ partOne input
    print $ partTwo input
