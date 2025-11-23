module Utils.Runner where

import           System.Environment

-- Main helpers
runDayPart :: (Show a, Show b) => (a -> b) -> (a -> b) -> a -> String -> IO ()
runDayPart _ _ input "parse"     = print input
runDayPart partOne _ input "one" = print . partOne $ input
runDayPart _ partTwo input "two" = print . partTwo $ input
runDayPart _ _ _ _               = error "Unknown part"

dayRunner :: (Show a, Show b) => (String -> a) -> (a -> b) -> (a -> b) -> IO [()]
dayRunner parseInput partOne partTwo = do
  args <- getArgs
  input <- parseInput <$> readFile (last args)
  mapM (runDayPart partOne partTwo input) $ init args

