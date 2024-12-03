module Main where

import qualified Day01 (parseInput, partOne, partTwo)
import qualified Day02 (parseInput, partOne, partTwo)
import System.Environment (getArgs)


inputDir :: String
inputDir = "/home/luke/Documents/code/advent-of-code/2024/input/"

getDayInputFile :: Int -> String
getDayInputFile day = let d = if day < 10 then "0" ++ show day else show day
                      in inputDir ++ "Day" ++ d ++ ".txt"


executeParts :: (Show a, Show b) => (String -> a) -> (String -> b) -> String -> IO ()
executeParts partOne partTwo input = do
    putStrLn "Part 1: "
    print partOne input
    putStrLn "\nPart 2: "
    print partTwo input
    putStrLn ""


main :: IO ()
main = do
    args <- getArgs
    let day = if null args then error "Please provide Day" else read (head args) :: Int
    putStrLn ("------ Day " ++ show day ++ "------")
    let inputFile = if length args == 1 then getDayInputFile day else args!!1
    putStrLn ("Using input file " ++ inputFile ++ "\n")
    input <- readFile inputFile
    case day of
         1 -> executeParts Day01.partOne Day01.partTwo $ Day01.parseInput input
         2 -> executeParts Day02.partOne Day02.partTwo $ Day02.parseInput input
         _ -> error "Invalid Day"
         -- TODO: can we do this dynamically?
