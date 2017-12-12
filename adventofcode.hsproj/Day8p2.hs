-- http://adventofcode.com/2017/day/8

module Day8p2 where
    
import Prelude hiding (LT, GT, EQ)
import Text.Regex.PCRE
import Text.Regex.Base.Context
import qualified Data.Map as Map

data Operation = Inc | Dec deriving (Show,Eq)
data Comparator = LT | GT | LTE | GTE | EQ | NEQ deriving (Show,Eq)

data Matched = Matched {
  register :: String, 
  operation :: Operation, 
  value :: Int, 
  ctrlRegister :: String, 
  ctrlOperation :: Comparator,
  ctrlValue :: Int
} deriving (Show)

type Registers = Map.Map String Int

toInt :: String -> Int
toInt x = read x :: Int

toOperation :: String -> Operation
toOperation "inc" = Inc
toOperation "dec" = Dec
toOperation unmatched = error $ "not a valid operation: " ++ unmatched

toComparator :: String -> Comparator
toComparator "<" = LT
toComparator ">" = GT
toComparator "<=" = LTE
toComparator ">=" = GTE
toComparator "==" = EQ
toComparator "!=" = NEQ
toComparator unmatched = error $ "not a valid comparator: " ++ unmatched

matchInstruction :: String -> [[String]]
matchInstruction line = line =~ "([a-z]+) (inc|dec) (\\-?\\d+) if ([a-z]+) (\\<|\\>|\\<=|\\>=|==|!=) (\\-?\\d+)" :: [[String]]

convert :: [[String]] -> Matched
convert matched = Matched (mEl !! 1) (toOperation (mEl !! 2)) (toInt (mEl !! 3)) (mEl !! 4) (toComparator (mEl !! 5))  (toInt (mEl !!6)) where mEl = matched !! 0

--convert :: [[String]] -> Matched
--convert ((register:operation:value:ctrlRegister:ctrlOperation:ctrlValue:[]):[]) = Matched register (toOperation operation) (toInt value) ctrlRegister ctrlOperation  (toInt ctrlValue)

checkCondition :: Int -> Comparator -> Int -> Bool
checkCondition a LT  b = a < b
checkCondition a GT  b = a > b
checkCondition a LTE b = a <= b
checkCondition a GTE b = a >= b
checkCondition a EQ  b = a == b
checkCondition a NEQ b = a /= b

updateRegisters :: Registers -> String -> Int -> Operation -> Int -> Registers
updateRegisters registers targetReg regValue Inc value = Map.insert targetReg (regValue + value) registers
updateRegisters registers targetReg regValue Dec value = Map.insert targetReg (regValue - value) registers


parse :: [String] -> [Matched]
parse lines = Prelude.map convert $ Prelude.map matchInstruction lines

process :: [Matched] -> Registers -> Int -> Int
process [] registers higestVal = maximum [higestVal,(mapMaxValue registers)]
process (x:xs) registers higestVal = let 
  regVal = Map.findWithDefault 0 (register x) registers
  ctrlRegVal = Map.findWithDefault 0 (ctrlRegister x) registers
  maxVal = maximum [higestVal,(mapMaxValue registers)]
  in 
    if checkCondition ctrlRegVal (ctrlOperation x) (ctrlValue x)
    then process xs (updateRegisters registers (register x) regVal (operation x) (value x)) maxVal
    else process xs registers maxVal

initProcess :: [Matched] -> Int
initProcess steps = process steps (Map.empty) 0

mapMaxValue :: Registers -> Int
mapMaxValue registers = if Map.null registers then 0 else maximum $ map snd $ Map.toList registers

solve input = initProcess $ parse $ lines input






