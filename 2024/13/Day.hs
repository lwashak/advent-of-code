module Day where

import           Utils.Runner (dayRunner)

import           Utils.Parsing    (readInts)

import           Data.Maybe (mapMaybe)

-- Types
type Input = [Claw]
type Output = Int

--                Button A | Button B | Prize
data Claw = Claw (Int, Int) (Int, Int) (Int, Int)
    deriving (Show, Read)

-- Parsing
parseInput :: String -> Input
parseInput raw =
    let xss = map readInts $ filter (/=[]) $ lines raw
    in getClaws xss

getClaws :: [[Int]] -> [Claw]
getClaws [] = []
getClaws (a@[_,_] : b@[_,_] : p@[_,_] : dss) = Claw (toPair a) (toPair b) (toPair p) : getClaws dss
    where toPair [x,y] = (x,y)
          toPair _     = error "Expecting 2 items"
getClaws _ = error "Parse Error!"


-- Solutions
partOne :: Input -> Output
partOne claws = let solutions = mapMaybe solveClaw claws
                    solutions' = filter (\(a,b) -> a <= 100 && b <= 100) solutions
                    costs = map (\(a,b) -> a*3 + b) solutions'
                in sum costs

partTwo :: Input -> Output
partTwo claws = let err = 10000000000000
                    claws' = map (\(Claw a b (c_x, c_y)) -> Claw a b (c_x + err, c_y + err)) claws
                    solutions = mapMaybe solveClaw claws'
                    costs = map (\(a,b) -> a*3 + b) solutions
                in sum costs

solveClaw :: Claw -> Maybe (Int, Int)
solveClaw (Claw a b c) =
    let d       = dotProduct a b
        dx      = dotProduct c b
        dy      = dotProduct a c
        (x,r_x) = dx `quotRem` d
        (y,r_y) = dy `quotRem` d
    in if r_x == 0 && r_y == 0 then Just (x,y)
                               else Nothing

dotProduct :: (Int, Int) -> (Int, Int) -> Int
dotProduct (a_x, a_y) (b_x, b_y) = (a_x * b_y) - (b_x * a_y)


-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo

