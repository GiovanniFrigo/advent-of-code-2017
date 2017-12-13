-- http://adventofcode.com/2017/day/3
-- Spiral Memory 

module Day3 where
import Data.List
import Data.Maybe
  
sqr x = x*x
leftDownDiag = [sqr (x*2-1) | x <- [1..]]

itemsPerRing = [x*8 | x <- [0..]]

-- in which ring is a number?
whichRing :: Int -> Int
-- in this case findIndex is safe, as leftDownDiag is an infinite list
whichRing x = fromJust $ findIndex (>= x) leftDownDiag

-- which is the biggest number on a ring?
ringBiggest ring = sqr $ ring * 2 + 1

distFromDiagonal :: Int -> Int -> Int
distFromDiagonal ringIx number = ((ringBiggest ringIx) - number) `mod` (ringIx * 2)

distFromLine :: Int -> Int -> Int
distFromLine ringIx number = abs $ (distFromDiagonal ringIx number) - ringIx

distFromCenter :: Int -> Int
distFromCenter 1 = 0 -- this is a special case
distFromCenter number = let ringIx = whichRing number in (ringIx + distFromLine ringIx number)