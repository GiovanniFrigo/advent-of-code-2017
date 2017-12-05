-- http://adventofcode.com/2017/day/2
module Day2 where 
  
toInt :: String -> Int
toInt x = read x :: Int

--toStringMatrix :: String -> [[String]]
--toStringMatrix input = map words $ lines input

--stringListToIntList :: [String] -> [Int]
--stringListToIntList list = [toInt x | x <- list]

toIntMatrix :: String -> [[Int]]
toIntMatrix input = map numbers (lines input)

-- words :: String -> [String]
numbers :: String -> [Int]
numbers input = map toInt (words input)

getMinMax :: [Int] -> (Int, Int)
getMinMax list = (minimum list, maximum list)

toMinMaxList :: [[Int]] -> [(Int, Int)]
toMinMaxList matrix = map getMinMax matrix

diff :: (Int,Int) -> Int
diff (a,b) = b - a

toDiffList :: [(Int, Int)] -> [Int]
toDiffList list = map diff list

solve input = foldl (+) 0 $ toDiffList $ toMinMaxList $ toIntMatrix input