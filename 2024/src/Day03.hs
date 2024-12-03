module Day03 () where

import Data.Char (isDigit)

type Mul = [Int]
type Input = [Mul]
type Output = Int

-- Parsing
parseMuls :: String -> [Mul]
parseMuls "" = []
parseMuls ('m':'u':'l':'(':cs) =
    let (x,cs1) = span isDigit cs
    in case cs1 of
        (',':cs2) -> let (y,cs3) = span isDigit cs2
                     in case cs3 of
                             (')':cs4) -> [read x,read y] : parseMuls cs4
                             _         -> parseMuls cs3
        _         -> parseMuls cs1
parseMuls (c:cs) = parseMuls cs

parseMulsWithDos :: Bool -> String -> [Mul]
parseMulsWithDos _ "" = []
parseMulsWithDos _ ('d':'o':'(':')':cs) = parseMulsWithDos True cs
parseMulsWithDos _ ('d':'o':'n':'\'':'t':'(':')':cs) = parseMulsWithDos False cs
parseMulsWithDos True ('m':'u':'l':'(':cs) =
    let (x,cs1) = span isDigit cs
    in case cs1 of
        (',':cs2) -> let (y,cs3) = span isDigit cs2
                     in case cs3 of
                             (')':cs4) -> [read x,read y] : parseMulsWithDos True cs4
                             _         -> parseMulsWithDos True cs3
        _         -> parseMulsWithDos True cs1
parseMulsWithDos d (c:cs) = parseMulsWithDos d cs

-- Solutions
partOne :: String -> Int
partOne = execute parseMuls

partTwo :: String -> Int
partTwo = execute (parseMulsWithDos True)

execute :: (String -> [Mul]) -> String -> Int
execute parser input =
    let ms = parser input
    in sum (map product ms)

-- Main
main :: IO ()
main = do
    raw <- readFile "../input/Day03.txt"
    print $ partOne raw
    print $ partTwo raw
