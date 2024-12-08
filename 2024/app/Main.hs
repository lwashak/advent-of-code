{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08

import System.Environment (getArgs)
import qualified System.Clock as C

parseArgs :: [String] -> (Int, String)
parseArgs [] = error "Expecting Day arg"
parseArgs [d] = (day, file)
    where day  = read d :: Int
          file = getInputFile day
parseArgs (d:f:_) = (day , f)
    where day  = read d :: Int

inputDir :: String
inputDir = "/home/luke/Documents/code/advent-of-code/2024/input/"

getInputFile :: Int -> String
getInputFile day = inputDir ++ "Day" ++ dayString ++ ".txt"
    where dayString = if day < 10 then "0" ++ show day else show day

runPart :: (Show a, Show b) => (String -> a) -> (a -> b) -> String -> IO (b, C.TimeSpec)
runPart parse part raw = do
    start <- C.getTime C.Monotonic
    let !result = part (parse raw)
    end <- C.getTime C.Monotonic
    return (result, C.diffTimeSpec start end)

printResult :: Show a => (a, C.TimeSpec) -> IO ()
printResult (result, time) = do
    let ms = C.toNanoSecs time `div` 1000000
    putStrLn $ show result ++ "\n(" ++ show ms ++ " ms)"

runDay :: (Show a, Show b) => (String -> a) -> [a -> b] -> String -> IO ()
runDay parse [partOne, partTwo] raw = do
    putStrLn "Part 1: "
    (r1, t1) <- runPart parse partOne raw
    printResult (r1, t1)
    putStrLn "\nPart 2: "
    (r2, t2) <- runPart parse partTwo raw
    printResult (r2, t2)
    return ()
runDay _ _ _ = error "Day should have 2 parts"

main :: IO ()
main = do
    args <- getArgs
    let (day, file) = parseArgs args
    raw <- readFile file
    putStrLn $ "----- Day " ++ show day ++ " -----"
    case day of
        1 -> runDay Day01.parseInput [Day01.partOne, Day01.partTwo] raw
        2 -> runDay Day02.parseInput [Day02.partOne, Day02.partTwo] raw
        3 -> runDay Day03.parseInput [Day03.partOne, Day03.partTwo] raw
        4 -> runDay Day04.parseInput [Day04.partOne, Day04.partTwo] raw
        5 -> runDay Day05.parseInput [Day05.partOne, Day05.partTwo] raw
        6 -> runDay Day06.parseInput [Day06.partOne, Day06.partTwo] raw
        7 -> runDay Day07.parseInput [Day07.partOne, Day07.partTwo] raw
        8 -> runDay Day08.parseInput [Day08.partOne, Day08.partTwo] raw
        _ -> error "Invalid Day"
        -- TODO: can we do this dynamically?
