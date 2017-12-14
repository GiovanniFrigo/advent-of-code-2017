-- http://adventofcode.com/2017/day/10
-- Knot Hash 

module Day10 where

-- knot given `count` values from `values` starting at position `offset`
knot :: [Int] -> Int -> Int -> [Int]
knot values count offset  = 
  let
    listLen = length values
    overCount = listLen - count
    overOffset = offset + count
  in select listLen (listLen - offset) $ (reverse (select count offset values)) ++ (select overCount overOffset values)
   
-- selects `count` elements from the infinitely cycled `list` starting at elem `offset`
select :: Int -> Int -> [Int] -> [Int]
select count offset list = take count $ drop offset $ cycle list

-- process instructions, values, current position, skipSize
process :: [Int] -> [Int] -> Int -> Int -> [Int]
process (x:xs) values position skipSize = 
  let
    knottedValues = knot values x position
    nextPosition = (position + x + skipSize) `mod` (length values)
  in process xs knottedValues nextPosition (skipSize+1)
process [] values _ _ = values


solve :: [Int] -> Int -> Int
solve input listSize = product $ take 2 $ process input [0..listSize] 0 0