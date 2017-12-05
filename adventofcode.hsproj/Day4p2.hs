-- http://adventofcode.com/2017/day/4

module Day4p2 where
import Data.List
import Data.Maybe

tokenize :: String -> [[String]]
tokenize input = map words $ lines input

countValid :: [[String]] -> Int
countValid lines = length $ filter (isValidPassphrase) lines

isValidPassphrase :: [String] -> Bool
isValidPassphrase words = isValidPassphrase' words

isValidPassphrase' :: [String] -> Bool
-- a passphrase is valid if the word we are examinating is not present another time in the rest of the list
isValidPassphrase' (x:xs) = isNothing (findIndex (\el -> (anagram el) == (anagram x)) xs) && isValidPassphrase' xs
isValidPassphrase' [] = True

-- "anagram" a word by sorting alphabetically its characters.
anagram :: [Char] -> [Char]
anagram word = sort word

solve input = countValid $ tokenize input