-- http://adventofcode.com/2017/day/9
-- Stream Processing

module Day9 where
    
parse :: [Char] -> Int -> Bool -> Int

parse ('{':xs) openedGroups False       = 0 + parse xs (openedGroups + 1) False -- '{' outside garbage opens a new group
parse ('}':xs) openedGroups False       = openedGroups + parse xs (openedGroups - 1) False -- '}' outside garbage closes a group: sum score
parse ('<':xs) openedGroups False       = 0 + parse xs openedGroups True -- enterning garbage (if not already inside it)
parse ('>':xs) openedGroups True        = 0 + parse xs openedGroups False -- exiting garbage
parse ('!':xs) openedGroups garbageMode = 0 + parse (tail xs) openedGroups garbageMode -- drop next character
parse ( _ :xs) openedGroups garbageMode = 0 + parse xs openedGroups garbageMode -- non relevant characters
parse []       _            _           = 0

process :: [Char] -> Int
process input = parse input 0 False

solve input = process input
