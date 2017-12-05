-- http://adventofcode.com/2017/day/1
module Day1 where

toIntegerArray :: Integer -> [Integer]
toIntegerArray list
    | list <= 0 = []
    | otherwise = (toIntegerArray (div list 10)) ++ [mod list 10]

process :: [Integer] -> Integer
process (a:b:xs) 
  | a == b  = a + process (b:xs)
  | otherwise = process (b:xs) 
process (a:xs) = 0
process [] = 0


cycleList :: [Integer] -> [Integer]
cycleList input = input ++ (take 1 input)


solve :: Integer -> Integer
solve input = process $ cycleList $ toIntegerArray input
