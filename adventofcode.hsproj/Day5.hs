-- http://adventofcode.com/2017/day/5
-- A Maze of Twisty Trampolines, All Alike

module Day5 where
import Data.List

toInt :: String -> Int
toInt x = read x :: Int

tokenize :: String -> [Int]
tokenize input = map toInt $ words input

inc :: Int -> Int
inc x = x + 1

-- return the list with the element at the specified index incremented by one
updateSteps :: [Int] -> Int -> [Int]
updateSteps list index = (take index list) ++ [inc (list !! index)] ++ (drop (index + 1) list)

parse :: [Int] -> Int
parse instructions = parse' instructions 0

parse' :: [Int] -> Int -> Int
parse' instructions index = 
  if index < 0 || index >= (length instructions) 
  then 0 
  else 1 + (parse' (updateSteps instructions index) updatedIndex)  
        where updatedIndex = index + (instructions !! index)

solve input = parse $ tokenize input