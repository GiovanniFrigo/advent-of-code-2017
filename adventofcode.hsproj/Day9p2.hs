-- http://adventofcode.com/2017/day/9
-- Stream Processing

module Day9p2 where
    
parse :: [Char] -> Bool -> Int

parse ('!':xs) garbageMode = 0 + parse (tail xs) garbageMode -- drop next character
parse ('<':xs) False       = 0 + parse xs True  -- enterning garbage (opening!! char does not count) 
parse ('>':xs) True        = 0 + parse xs False -- exiting garbage (closing char does not count) 
parse ( _ :xs) True        = 1 + parse xs True  -- inside garbage: count character
parse ( _ :xs) False       = 0 + parse xs False -- else skip it
parse []       _           = 0

process :: [Char] -> Int
process input = parse input False

solve input = process input