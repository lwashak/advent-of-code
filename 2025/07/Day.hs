module Day where

import Utils.Runner (dayRunner)
import Utils.Grid (Pos, zipCoords)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- Types
type Grid = Map Pos Char
type Input = (Grid, Pos)
type Output = Int

-- Parsing
parseInput :: String -> Input
parseInput input = (grid, startingPos)
  where grid = Map.fromList . zipCoords . lines $ input
        startingPos = (fromJust $ elemIndex 'S' input, 0)

-- Solutions
fireTachyonBeam :: [Pos] -> Grid -> Pos -> [Pos]
fireTachyonBeam visited grid (x,y) = case Map.lookup (x,y+1) grid of
  Just '^' -> if (x,y+1) `elem` visited then
    visited
  else
    let visited'  = fireTachyonBeam ((x,y+1) : visited) grid (x+1,y+1)
    in fireTachyonBeam visited' grid (x-1,y+1)
  Just '.' -> fireTachyonBeam visited grid (x,y+1)
  _        -> visited

fireQuantumTachyonBeam :: Map Pos Int -> Grid -> Pos -> (Int, Map Pos Int)
fireQuantumTachyonBeam visited grid (x,y) = case Map.lookup (x,y+1) grid of
  Just '^' -> case Map.lookup (x,y+1) visited of
    Just t -> (t, visited)
    Nothing -> let (timelinesL, visitedL) = fireQuantumTachyonBeam visited grid (x-1,y+1)
                   (timelinesLR, visitedLR) = fireQuantumTachyonBeam visitedL grid (x+1,y+1)
                   t = 1 + timelinesL + timelinesLR
                   visited' = Map.insert (x,y+1) t visitedLR
               in (t, visited')
  Just '.' -> fireQuantumTachyonBeam visited grid (x,y+1)
  _ -> (0, visited)

partOne :: Input -> Output
partOne (grid, pos) = length . fireTachyonBeam [] grid $ pos

partTwo :: Input -> Output
partTwo (grid, pos) = (+1) . fst . fireQuantumTachyonBeam Map.empty grid $ pos

-- Main
main :: IO [()]
main = dayRunner parseInput partOne partTwo
