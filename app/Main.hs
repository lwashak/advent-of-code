module Main where

import qualified Day01 (part1, part2)
import qualified Day02 (part1, part2)
import System.Environment (getArgs)

import System.Clock (getTime, diffTimeSpec, Clock(Monotonic))


inputDir :: String
inputDir = "/home/luke/Code/advent/aoc2024/input/"

getDayInputFile :: Int -> String
getDayInputFile day = let d = if day < 10 then "0" ++ show day else show day
                      in inputDir ++ "Day" ++ d ++ ".txt"

executePart :: (Show a) => (String -> a) -> String -> IO String
executePart part input = do
    start <- getTime Monotonic
    let result = part input
    end <- getTime Monotonic
    let time = diffTimeSpec start end
    return $ show result ++ "\n" ++ show time


executeParts :: (Show a, Show b) => (String -> a) -> (String -> b) -> String -> IO ()
executeParts part1 part2 input = do
    putStrLn "Part 1: "
    output1 <- executePart part1 input
    putStrLn output1
    putStrLn "\nPart 2: "
    output2 <- executePart part2 input
    putStrLn output2
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
         1 -> executeParts Day01.part1 Day01.part2 input
         2 -> executeParts Day02.part1 Day02.part2 input
         _ -> error "Invalid Day"
         -- TODO: can we do this dynamically?
