module Common (splitOn, insertIntoSorted) where

import Data.Char (isDigit)

-- getFirstInt :: String -> (Maybe Int, String)
-- getFirstInt s =
--     let (_,s') = break isDigit s
--         (ds, s'') = span isDigit s'
--         x = if null ds then Nothing else Just (read ds :: Int)
--     in (x, s'')

-- quickSort :: Ord a => [a] -> [a]
-- quickSort [] = []
-- quickSort (x:xs) =
--     let smallerSorted = quickSort $ filter (<=x) xs
--         biggerSorted = quickSort $ filter (>x) xs
--     in smallerSorted ++ [x] ++ biggerSorted


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn y xs = case break (==y) xs of
    (ls, [])   -> [ls]
    (ls, _:rs) -> ls : splitOn y rs


insertIntoSorted :: Int -> [Int] -> [Int]
insertIntoSorted x [] = [x]
insertIntoSorted x (y:ys) =
    if x <= y
    then x:y:ys
    else y : insertIntoSorted x ys
