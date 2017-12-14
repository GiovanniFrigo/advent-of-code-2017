-- http://adventofcode.com/2017/day/10
-- Knot Hash 

module Day10p2 where

import Data.Char
import Data.Bits
import Data.List.Split
import Numeric

data HashTypeResult = HashTypeResult {
  resultBits :: [Int],
  resultPosition :: Int,
  resultSkipSize :: Int
} deriving (Show)

-- xor all the elements of a list
xorr :: [Int] -> Int
xorr = foldl (xor) 0

-- convert a string to a list of each char's ascii code
toAsciiString :: String -> [Int]
toAsciiString = map ord

-- convert int to 2-char hex representation string
toHex :: Int -> String
toHex val = reverse $ take 2 $ reverse ("00" ++ showHex val "")

knotMagic = [17,31,73,47,23]

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

-- complete a single round in the hashKnot algorithm
hashKnotRound :: [Int] -> [Int] -> Int -> Int -> HashTypeResult
hashKnotRound (x:xs) values position skipSize = 
  let
    knottedValues = knot values x position
    nextPosition = (position + x + skipSize) `mod` (length values)
  in hashKnotRound xs knottedValues nextPosition (skipSize+1)
hashKnotRound [] values position skipSize = HashTypeResult values position skipSize

-- calculate the sparse hash by executing the single round a number of times
sparseHash' :: [Int] -> [Int] -> Int -> Int -> Int -> [Int]
sparseHash' _ values 0 _ _ = values
sparseHash' instructions values pass position skipSize = 
  let 
    result = hashKnotRound instructions values position skipSize
   in sparseHash' instructions (resultBits result) (pass-1) (resultPosition result) (resultSkipSize result)

-- sparseHash' initiator
sparseHash :: String -> [Int]
sparseHash input = 
  let 
    initialState = toAsciiString input ++ knotMagic
  in sparseHash' initialState [0..255] 64 0 0
  
-- condense the sparse hash from 256 Int to 16 Int by xor-ring 16 elements at a time
condenseHash :: [Int] -> [Int]
condenseHash sparse = map xorr $ chunksOf 16 sparse

hash :: String -> [Int]
hash input = condenseHash $ sparseHash input

hashToString :: [Int] -> String
hashToString hash = concat $ map toHex hash

solve :: String -> String
solve input = hashToString $ hash input
