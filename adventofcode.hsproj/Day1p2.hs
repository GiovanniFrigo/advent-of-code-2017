-- http://adventofcode.com/2017/day/1
-- Inverse Captcha

module Day1p2 where

toIntegerArray :: Integer -> [Integer]
toIntegerArray list
    | list <= 0 = []
    | otherwise = (toIntegerArray (div list 10)) ++ [mod list 10]


process :: [Integer] -> Integer
process list = process' list 0


process' :: [Integer] -> Int -> Integer
process' list index 
  | index < length list = 
    if (hasDouble list index)
    then list !! index + process' list (index+1)
    else process' list (index+1)
  | otherwise = 0


getCycledItem :: [Integer] -> Int -> Integer
getCycledItem list index = cycle list !! index


hasDouble :: [Integer] -> Int -> Bool
hasDouble list index = getCycledItem list index == getCycledItem list (index + halvedLength list)


halvedLength :: [Integer] -> Int
halvedLength list = length list `quot` 2


solve :: Integer -> Integer
solve input = process $ toIntegerArray input


-- my manual implementation of getCycledItem, before thinking about it and using `cycle`
-- getItem :: [Integer] -> Integer -> Integer
-- getItem list index 
--   | index > listLen = list !! (index - listLen - 1)
--   | otherwise = list !! index
--   where listLen = length list - 1