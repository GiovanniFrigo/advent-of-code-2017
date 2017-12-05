-- http://adventofcode.com/2017/day/2

module Day2p2 where 
import Data.List -- for find
import Data.Maybe -- for isNothing
  
toInt :: String -> Int
toInt x = read x :: Int

toIntMatrix :: String -> [[Int]]
toIntMatrix input = map numbers (lines input)

-- words :: String -> [String]
numbers :: String -> [Int]
numbers input = map toInt (words input)

-- false until it finds an a that divides b and is not the same number
divisor :: Int -> Int -> Bool
divisor a b = (b `mod` a == 0) && (a /= b)

findDivisor :: [Int] -> Int -> Maybe Int
--findDivisor list number =
--  let divisorList = filter (divisor number) list
--  in
--    if null divisorList
--    then Nothing 
--    else Just (head divisorList)
findDivisor list number = find (divisor number) list

getEvenlyDivisible :: Int -> [Int] -> (Int, Int)
getEvenlyDivisible index list = 
  if index > length list 
  then error "Not found" 
  else (
    case divisor of
    Just d -> ((list !! index),d)
    Nothing -> getEvenlyDivisible (index+1) list
  ) where divisor = findDivisor list (list !! index)

toEvenlyDivisibleList :: [[Int]] -> [(Int, Int)]
toEvenlyDivisibleList matrix = map (getEvenlyDivisible 0) matrix

divide :: (Int,Int) -> Int
divide (a,b) = b `div` a

toDivideList :: [(Int, Int)] -> [Int]
toDivideList list = map divide list

solve input = foldl (+) 0 $ toDivideList $ toEvenlyDivisibleList $ toIntMatrix input