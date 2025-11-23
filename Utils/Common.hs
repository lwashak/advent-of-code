module Utils.Common (splitOn, insertIntoSorted, findIndices2D) where


-- Matrix / 2D List Operations
findIndices2D :: Eq a => (a -> Bool) -> [[a]] -> [(Int,Int)]
findIndices2D = findIndices2D' (0,0)

findIndices2D' :: Eq a => (Int,Int) -> (a -> Bool) -> [[a]] -> [(Int,Int)]
findIndices2D' _ _ [] = []
findIndices2D' (_,j) f ([]:xss) = findIndices2D' (0,j+1) f xss
findIndices2D' (i,j) f ((x:xs):xss)
    | f x       = (i,j) : findIndices2D' (i+1, j) f (xs:xss)
    | otherwise = findIndices2D' (i+1, j) f (xs:xss)


-- List Operations
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn y xs = case break (==y) xs of
    (ls, [])   -> [ls]
    (ls, _:rs) -> ls : splitOn y rs

insertIntoSorted :: Ord a => a -> [a] -> [a]
insertIntoSorted x [] = [x]
insertIntoSorted x (y:ys) =
    if x <= y
    then x:y:ys
    else y : insertIntoSorted x ys

