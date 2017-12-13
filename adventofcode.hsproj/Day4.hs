-- http://adventofcode.com/2017/day/4
-- High-Entropy Passphrases

module Day4 where
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
isValidPassphrase' (x:xs) = isNothing (findIndex (\el -> el == x) xs) && isValidPassphrase' xs
isValidPassphrase' [] = True

solve input = countValid $ tokenize input