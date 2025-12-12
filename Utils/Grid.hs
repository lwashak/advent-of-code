module Utils.Grid (NESW4, Pos, zipCoords, getSurroundingCoords4, getSurroundingCoords8) where

data NESW4 = N | E | S | W deriving (Show,Eq)
-- data NESW8 = N | NE | E | SE | S | SW | W | NW deriving (Show,Eq)

type Pos = (Int, Int)


zipCoords :: [[a]] -> [(Pos,a)]
zipCoords = zipCoords' 0

zipCoords' :: Int -> [[a]] -> [(Pos,a)]
zipCoords' _ [] = []
zipCoords' j (xs:xss) = zip coords xs ++ zipCoords' (j+1) xss
  where coords = map (,j) [0..]

getSurroundingCoords4 :: Pos -> [Pos]
getSurroundingCoords4 (i,j) = [(i+1,j),(i-1,j),(i,j+1),(i,j-1)]

getSurroundingCoords8 :: Pos -> [Pos]
getSurroundingCoords8 (i,j) =
  [(i,j-1),(i+1,j-1),(i+1,j),(i+1,j+1),(i,j+1),(i-1,j+1),(i-1,j),(i-1,j-1)]

