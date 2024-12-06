module Day03 () where

import Data.Char (isDigit)

import qualified Text.Parsec as P (string, char, digit, many, many1, choice, try, anyToken, putState, getState, manyTill, skipMany, optional, option, lookAhead, parse)
import Text.Parsec (Parsec, (<|>))

-- Types
type Input = [Op]
type Output = Int

data Op = Do
        | Dont
        | Mul Int Int
        deriving (Show)

-- Parsing
parseInput :: String -> [Op]
parseInput [] = []
parseInput s@(_:cs) = case P.parse operatorParser "" s of
    Right x -> x : parseInput cs
    _       -> parseInput cs

numberParser :: Parsec String () String
numberParser = P.many1 P.digit

mulParser :: Parsec String () Op
mulParser = do
    _ <- P.string "mul("
    x <- numberParser
    _ <- P.char ','
    y <- numberParser
    _ <- P.char ')'
    return $ Mul (read x) (read y)

doParser :: Parsec String () Op
doParser = do
    P.try $ P.string "do()"
    return Do

dontParser :: Parsec String () Op
dontParser = do
    P.try $ P.string "don't()"
    return Dont

operatorParser :: Parsec String () Op
operatorParser = P.choice [doParser, dontParser, mulParser]


-- Solutions
partOne :: [Op] -> Int
partOne ops = sum [ x * y | Mul x y <- ops ]

partTwo :: Input -> Int
partTwo = evaluateProgram True

evaluateProgram :: Bool -> [Op] -> Int
evaluateProgram _ [] = 0
evaluateProgram _ (Do:ops) = evaluateProgram True ops
evaluateProgram _ (Dont:ops) = evaluateProgram False ops
evaluateProgram True ((Mul x y):ops) = x * y + evaluateProgram True ops
evaluateProgram enabled (_:ops) = evaluateProgram enabled ops

-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day03.txt"
    let input = parseInput raw
    print $ partOne input
    print $ partTwo input
