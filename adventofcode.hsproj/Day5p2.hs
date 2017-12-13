-- http://adventofcode.com/2017/day/5
-- A Maze of Twisty Trampolines, All Alike

module Day5p2 where
import Data.List
import Data.Char

toInt :: String -> Int
toInt x = read x :: Int

tokenize :: String -> [Int]
tokenize input = map toInt $ words input

-- the only thing that changed from part one is the function that modifies the array. It's no longer an increment but rather an increment or decrement depending on the value of the current memory cell
updateCell :: Int -> Int
updateCell x = if (x >= 3) then x - 1 else x + 1

-- return the list with the element at the specified index incremented by one
updateSteps :: [Int] -> Int ->[Int]
updateSteps list index = (take index list) ++ [updateCell (list !! index)] ++ (drop (index + 1) list)

parse :: [Int] -> Int
parse instructions = parse' instructions 0

parse' :: [Int] -> Int -> Int
parse' instructions index = 
  if index < 0 || index >= (length instructions) 
  then 0 
  else 1 + (parse' (updateSteps instructions index) updatedIndex)  
        where updatedIndex = index + (instructions !! index)

solve input = parse $ tokenize input

-- For some reasons, running solve in the Haskell editor makes it hang (possibly stack overflow).
-- Decomment the following lines and build an executable with `ghc [Day5p2.hs]`, then call it with `cat input.txt | ./Day5p2"
-- spoiler: the answer is 25136209

--main = do  
--    input <- getContents  
--    print "Got input! Processing..."  
--    print $ show $ solve input
