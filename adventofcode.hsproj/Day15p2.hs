-- http://adventofcode.com/2017/day/15
-- Dueling Generators

module Day15p2 where
import Data.Bits
  

-- constants
generatorAfactor = 16807
generatorBfactor = 48271


getNextVal val factor check = 
  if val `mod` check == 0
  then val 
  else getNextVal ((val * factor) `mod` 2147483647) factor check

generate :: Int -> Int -> Int -> Int
generate _    _    0      = 0
generate valA valB passes = 
  let
     nextA = getNextVal ((valA * generatorAfactor) `mod` 2147483647) generatorAfactor 4
     nextB = getNextVal ((valB * generatorBfactor) `mod` 2147483647) generatorBfactor 8
     matches = (nextA .&. 0xffff) == (nextB .&. 0xffff)
     count = if matches then 1 else 0
  in count + generate nextA nextB (passes-1)
  


solve seedA seedB passes = generate seedA seedB passes 