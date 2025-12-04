module Utils.Numbers (countDigits, intToDigits, digitsToInt, appendInts, append2Ints) where

countDigits :: Int -> Int
countDigits 0 = 0
countDigits x = 1 + countDigits (div x 10)

intToDigits :: Int -> [Int]
intToDigits x
  | x < 0 = intToDigits (-x)
  | x < 10 = [x]
  | otherwise = intToDigits x' ++ [d] 
  where (x', d) = x `quotRem` 10

digitsToInt :: [Int] -> Int
digitsToInt = digitsToInt' 0
  where digitsToInt' :: Int -> [Int] -> Int
        digitsToInt' acc [] = acc
        digitsToInt' acc (d:ds) = let acc' = acc * 10 + d in digitsToInt' acc' ds


append2Ints :: Int -> Int -> Int
append2Ints x y = digitsToInt (xDigits ++ yDigits)
  where xDigits = intToDigits x
        yDigits = intToDigits y

appendInts :: [Int] -> Int
appendInts = digitsToInt . concatMap intToDigits
