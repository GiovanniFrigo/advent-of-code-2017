-- http://adventofcode.com/2017/day/11
-- Hex Ed

module Day11 where
import Data.List.Split

data Coord = Coord {
  xCoord :: Int,
  yCoord :: Int,
  zCoord :: Int
} deriving (Show)
   
(Coord xA yA zA) |+| (Coord xB yB zB) = Coord (xA + xB) (yA + yB) (zA + zB)


offset :: String -> Coord
offset "n"  = Coord 0 1 (-1)
offset "ne" = Coord 1 0 (-1) 
offset "se" = Coord 1 (-1) 0
offset "s"  = Coord 0 (-1) 1
offset "sw" = Coord (-1) 0 1
offset "nw" = Coord (-1) 1 0
offset unmatched = error $ "unknown direction: " ++ unmatched

walk :: [String] -> Coord -> Coord
walk (x:xs) position = walk xs $ position |+| offset x
walk []     position = position

distance :: Coord -> Int
distance position = 
  maximum  [abs (xCoord position), abs (yCoord position), abs (zCoord position)]

toDirections :: String -> [String]
toDirections = splitOn "," 

parse :: [String] -> Int
parse directions = distance $ walk directions (Coord 0 0 0)

solve :: String -> Int
solve input = parse $ toDirections input