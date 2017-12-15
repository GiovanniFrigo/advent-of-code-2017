-- http://adventofcode.com/2017/day/15
-- Dueling Generators

module Day15 where
import Data.Bits
  

-- constants
generatorAfactor = 16807
generatorBfactor = 48271

generate :: Int -> Int -> Int -> Int
generate _    _    0      = 0
generate valA valB passes = 
  let
     nextA = (valA * generatorAfactor) `mod` 2147483647
     nextB = (valB * generatorBfactor) `mod` 2147483647
     matches = (nextA .&. 0xffff) == (nextB .&. 0xffff)
     count = if matches then 1 else 0
  in count + generate nextA nextB (passes-1)
  


solve seedA seedB passes = generate seedA seedB passes 