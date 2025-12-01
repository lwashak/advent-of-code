module Day where

import Utils.Runner (dayRunner)

-- Types
data Rotation = L Int | R Int deriving (Show)
type Input = [Rotation]
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = map parseRotation . lines

parseRotation :: String -> Rotation
parseRotation ('L':cs) = L (read cs)
parseRotation ('R':cs) = R (read cs)
parseRotation s = error $ "Parse error" ++ s

-- Solutions
partOne :: Input -> Output
partOne = length . filter ((==0).fst) . enterCode 50

partTwo :: Input -> Output
partTwo = sum . map snd . enterCode 50

-- Given a starting position, and rotation, return ending position and number of clicks
rotate :: Int -> Rotation -> (Int,Int)
rotate 0 (L x) = rotate 100 (L x)
rotate p (L x) = rotate p (R (-x))
rotate p (R x) = (p', c')
  where (c,p') = (p + x) `divMod` 100
        c' = if x < 0 && p' == 0 then abs c + 1 else abs c

enterCode :: Int -> [Rotation] -> [(Int,Int)]
enterCode _ [] = []
enterCode p (r:rs) = (p',clicks) : enterCode p' rs
  where (p',clicks) = rotate p r

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo

