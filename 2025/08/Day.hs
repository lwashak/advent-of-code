module Day where

import           Data.List     (delete, find, nub, sort, sortOn)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (catMaybes)
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Utils.Parsing (readInts)
import           Utils.Runner  (dayRunner)

-- Types
type Pos = (Int, Int, Int)
type Input = [Pos]
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput = map (listToTuple3 . readInts) . lines
  where listToTuple3 [x,y,z] = (x,y,z)
        listToTuple3 _       = error "Parse Error"

-- Solutions
distance :: Pos -> Pos -> Float
distance (x1,y1,z1) (x2,y2,z2) = sqrt . fromIntegral $ xds + yds + zds
  where xds = (x1-x2)^(2 :: Integer)
        yds = (y1-y2)^(2 :: Integer)
        zds = (z1-z2)^(2 :: Integer)

lookupDistance :: Map (Pos,Pos) Float -> Pos -> Pos -> Maybe Float
lookupDistance computed a b = case Map.lookup (a,b) computed of
  Nothing -> Map.lookup (b,a) computed
  Just d  -> Just d

computeDistances :: Map (Pos,Pos) Float -> [Pos] -> Pos -> Map (Pos,Pos) Float
computeDistances computed [] _ = computed
computeDistances computed (b:ps) a = case lookupDistance computed a b of
  Just _ -> computeDistances computed ps a
  Nothing -> computeDistances computed' ps a
    where computed' = Map.insert (a,b) (distance a b) computed

closest :: Map (Pos,Pos) Float -> [Pos] -> [((Pos, Pos), Float)]
closest computed [] = sortOn snd . Map.toList $ computed
closest computed (p:ps) = closest computed' ps
  where computed' = computeDistances computed ps p

placeInCircuit :: [Set Pos] -> Pos -> Pos -> [Set Pos]
placeInCircuit cs a b = case exisitingCircuits of
  [c]     -> Set.union c abSet : delete c cs
  [c1,c2] -> Set.unions [c1, c2, abSet] : (delete c1 . delete c2) cs
  _       -> abSet : cs
  where aCircuit = find (Set.member a) cs
        bCircuit = find (Set.member b) cs
        exisitingCircuits = nub $ catMaybes [aCircuit, bCircuit]
        abSet = Set.fromList [a, b]

makeCircuits :: [Set Pos] -> [((Pos,Pos), Float)] -> [Set Pos]
makeCircuits cs [] = cs
makeCircuits cs (((a,b),_):ps) = makeCircuits cs' ps
  where cs' = placeInCircuit cs a b

completeCircuit :: Int -> [Set Pos] -> [((Pos,Pos), Float)] -> (Pos,Pos)
completeCircuit _ _ [] = error "Can't complete circuit"
completeCircuit n cs (((a,b),_):ps)
  | length cs' == 1 && Set.size (head cs') == n = (a,b)
  | otherwise = completeCircuit n cs' ps
  where cs' = placeInCircuit cs a b

partOne :: Input -> Output -- x < 201372000
partOne = product . take 3 . reverse . sort . map Set.size . makeCircuits [] . take 1000 . closest Map.empty

partTwo :: Input -> Output
partTwo = (\((x1,_,_),(x2,_,_)) -> x1*x2) . completeCircuit 1000 [] . closest Map.empty

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo

