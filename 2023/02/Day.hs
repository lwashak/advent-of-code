module Day where

import           Utils.Runner (dayRunner)
import Data.List (isPrefixOf)
import Data.Char (isDigit)

-- Types
type Game = (Int, [(Int, Int, Int)])
type Input = [Game]
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = map parseGameAndId . lines

parseGameAndId :: String -> Game
parseGameAndId xs
  | x == "" = parseGameAndId (tail xs')
  | otherwise = (read x, parseGame [] xs')
  where (x,xs') = span isDigit xs

parseGame :: [(Int, Int, Int)] -> String -> [(Int, Int, Int)]
parseGame [] xs = parseGame [(0,0,0)] xs
parseGame games "" = games
parseGame games (' ':xs) = parseGame games xs
parseGame games (',':xs) = parseGame games xs
parseGame (game:games) (';':xs) = game : parseGame games xs
parseGame ((r,g,b):games) xs
  | x == "" = parseGame ((r,g,b):games) (tail xs')
  | " red" `isPrefixOf` xs' = let r' = r + read x in parseGame ((r',g,b):games) xs'
  | " green" `isPrefixOf` xs' = let g' = g + read x in parseGame ((r,g',b):games) xs'
  | " blue" `isPrefixOf` xs' = let b' = b + read x in parseGame ((r,g,b'):games) xs'
  where (x,xs') = span isDigit xs
parseGame _ xs = error $ "Parse Error: " ++ xs


-- Solutions
partOne :: Input -> Output
partOne = sum . map fst . filter (isGamePossible (12,13,14))

partTwo :: Input -> Output
partTwo = sum . map (powerCubes . minNumberOfCubes (0,0,0))

isGamePossible :: (Int, Int, Int) -> Game -> Bool
isGamePossible _ (_,[]) = True
isGamePossible (rt,gt,bt) (i,(r,g,b):xs)
  | r > rt || g > gt || b > bt = False
  | otherwise = isGamePossible (rt,gt,bt) (i,xs)

minNumberOfCubes :: (Int, Int, Int) -> Game -> (Int, Int, Int)
minNumberOfCubes cubes (_,[]) = cubes
minNumberOfCubes (rt,gt,bt) (i,(r,g,b):xs) = minNumberOfCubes (rt',gt',bt') (i,xs)
  where rt' = max rt r
        gt' = max gt g
        bt' = max bt b


powerCubes :: (Int, Int, Int) -> Int
powerCubes (r,g,b) = r * g * b


-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo
