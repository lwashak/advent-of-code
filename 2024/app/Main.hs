module Main where

import qualified Day01 (parseInput, partOne, partTwo)
import qualified Day02 (parseInput, partOne, partTwo)
import System.Environment (getArgs)


inputDir :: String
inputDir = "/home/luke/Documents/code/advent-of-code/2024/input/"

getInputFile :: Int -> String
getInputFile day = let d = if day < 10 then "0" ++ show day else show day
                   in inputDir ++ "Day" ++ d ++ ".txt"


main :: IO ()
main = do
    putStrLn "Enter Day: "
    day <- getLine
    let dayInt = read day :: Int
    let defaultFile = getInputFile dayInt
    putStrLn $ "Enter Input File (default=" ++ show defaultFile ++ "): "
    fileInput <- getLine
    let file = if null fileInput then defaultFile else fileInput
    raw <- readFile file
    case dayInt of
         1 -> print $ Day01.partOne $ Day01.parseInput raw
         2 -> print $ Day02.partOne $ Day02.parseInput raw
         _ -> error "Invalid Day"
         -- TODO: can we do this dynamically?
