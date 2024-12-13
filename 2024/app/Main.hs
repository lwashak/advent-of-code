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
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12

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

timeFunction :: (a -> b) -> a -> IO (b, C.TimeSpec)
timeFunction f x = do
    start <- C.getTime C.Monotonic
    let !result = f x
    end <- C.getTime C.Monotonic
    return (result, C.diffTimeSpec start end)

printResult :: Show a => (a, C.TimeSpec) -> IO ()
printResult (result, time) = do
    let resultString = show result
    if length resultString > 200
       then putStrLn $ take 200 resultString ++ "..."
       else putStrLn resultString
    let microsecs = fromIntegral (C.toNanoSecs time) / 1000000.0 :: Double
    putStrLn $ "(" ++ show microsecs ++ " ms)"

runDay :: (Show a, Show b) => (String -> a) -> [a -> b] -> String -> IO ()
runDay parse [partOne, partTwo] raw = do
    putStrLn "Parsed Input: "
    (input, t0) <- timeFunction parse raw
    printResult (input, t0)
    putStrLn "\nPart 1: "
    (r1, t1) <- timeFunction partOne input
    printResult (r1, t1)
    putStrLn "\nPart 2: "
    (r2, t2) <- timeFunction partTwo input
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
        1  -> runDay Day01.parseInput [Day01.partOne, Day01.partTwo] raw
        2  -> runDay Day02.parseInput [Day02.partOne, Day02.partTwo] raw
        3  -> runDay Day03.parseInput [Day03.partOne, Day03.partTwo] raw
        4  -> runDay Day04.parseInput [Day04.partOne, Day04.partTwo] raw
        5  -> runDay Day05.parseInput [Day05.partOne, Day05.partTwo] raw
        6  -> runDay Day06.parseInput [Day06.partOne, Day06.partTwo] raw
        7  -> runDay Day07.parseInput [Day07.partOne, Day07.partTwo] raw
        8  -> runDay Day08.parseInput [Day08.partOne, Day08.partTwo] raw
        9  -> runDay Day09.parseInput [Day09.partOne, Day09.partTwo] raw
        10 -> runDay Day10.parseInput [Day10.partOne, Day10.partTwo] raw
        11 -> runDay Day11.parseInput [Day11.partOne, Day11.partTwo] raw
        12 -> runDay Day12.parseInput [Day12.partOne, Day12.partTwo] raw
        _ -> error "Invalid Day"
        -- TODO: can we do this dynamically?
