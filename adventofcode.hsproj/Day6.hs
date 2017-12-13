-- http://adventofcode.com/2017/day/6
-- Memory Reallocation

module Day6 where
import Data.List
import Data.Maybe

toInt :: String -> Int
toInt x = read x :: Int

tokenize :: String -> [Int]
tokenize input = map toInt $ words input

toString :: [Int] -> String
toString list = concatMap (\x -> (show x) ++ ",") list

parse :: [Int] -> Int
parse state = parse' state []

parse' :: [Int] -> [String] -> Int
parse' state alreadySeenDispositions = 
  if currentDisposition `elem` alreadySeenDispositions
  then 0 
  else 1 + (parse' (doStep state) (alreadySeenDispositions ++ [currentDisposition]))  
        where currentDisposition = toString state

-- returns a new list in which the n-th element is replaced by the given value
replaceItemAtPos :: [Int] -> Int -> Int -> [Int]
replaceItemAtPos list pos newValue = (take pos list) ++ [newValue] ++ (drop (pos + 1) list)

-- rolls the index of a list between [0, length list)
rollIndex :: Int -> [Int] -> Int
rollIndex index list = if index >= length list then 0 else index

-- the doStep finds the bank with the most blocks, gets its index, zeroes that bank,
-- calculates how many blocks should be put in each bank 
-- and finally proceeds with distributing the amount in the banks
doStep :: [Int] -> [Int]
doStep list = let
    amountToDistribute = maximum list
    maxPosition = fromJust $ findIndex (\x -> amountToDistribute == x) list
    len = length list
    valuePerCell = ceiling $ (fromIntegral amountToDistribute) / (fromIntegral len)
  in distribute (replaceItemAtPos list maxPosition 0) amountToDistribute valuePerCell (rollIndex (maxPosition + 1) list)
    
distribute :: [Int] -> Int -> Int -> Int -> [Int]
distribute list amountToDistribute valuePerCell index = 
  if amountToDistribute <= 0 
  then list
  else distribute (replaceItemAtPos list index (amount + list !! index)) (amountToDistribute - valuePerCell) valuePerCell (rollIndex (index + 1) list)
  where amount = min amountToDistribute valuePerCell

solve input = parse $ tokenize input