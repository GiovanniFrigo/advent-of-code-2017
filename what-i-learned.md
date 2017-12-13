# What I learned
#### by trial and error

## Days 1 - 5

The challenges of the first days were just a warm-up, just like last year.

Getting used to the Haskell's way of parsing inputs (usually given as strings or numbers which need to be tokenized into lists to be used), using pattern matching on parameters, benefitting from list operations.

Also introducing: the Maybe monad. 😱

All clear! 🚀

## Day 6

Today I felt like I'm really _"just making things work"_ and definitely not doing them the right way.

The challenge required to redistribute some integer values in a list, which implied I had to pass to each recursive function call a modified version of the [Int] list received as input. 

Moreover, I needed to keep track of the already seen distributions, for which I chose to convert the [Int] list to a String (`concatMap (\x -> (show x) ++ ",") list` - why the hell didn't I just use `show list`?)  and then pass it to the processing function, appended to the alreay seen configurations [Strings] list. Not the most optimized way of doing it, I think, as the list is growing bigger by 1 element at each processing step! 

In this case, having the lists as variables would be probably an easier solution, but I'm too new to the language to know how to do it (monads, maybe?).

The benefit of using a list of strings to remember the already seen configurations came up in part two, where we needed to identify how many steps ago the last configuration was seen. Would have been definitely harder if I had used HashSets!

## Day 8
..I'm starting to fall behind as the days go on and on! 😞

I decided to skip day 7 and to do day 8 before it as both required regular expressions to be solved, but day 7 also needs trees on top of that. Not knowing how to do either things in Haskell, I figured out it was a good idea to start from day 8, which apparently can be solved with a Regex and a little more magic.

In the end, I introduced so many new things in today's script that I did not imagine!

#### Regexes
So, a regex matches the given instructions (For example `b inc 5 if a > 1` or ` dec -10 if a >= 1`), returning a [String] list.

Pretty easy to do it with the `Text.Regex.PCRE` module and the `=~` operator, despite at the beginning it wasn't clear that it was such a polymorphic operator depending on which types the result is casted to (`Bool` to check for a match, `Int` to count matches, `String` to return the first match, `[[String]]` to return all matches and all captured groups, ..)

#### Data structures
Accessing the 6 different parts of the instruction when needed was definitely going to be complicated if I stored them as Strings, so i decided to build my first-ever data structure to hold the different parts!

```haskell
data Matched = Matched {
  register :: String, 
  operation :: Operation, 
  value :: Int, 
  ctrlRegister :: String, 
  ctrlOperation :: Comparator,
  ctrlValue :: Int
} deriving (Show)
```

How about creating custom types for the operation to be executed (that could only be `inc` or `dec`) and the comparator (one of `<`, `>`, `<=`, `>=`, `==`, `!=`)? Here we go:

```haskell
data Operation  = Inc | Dec deriving (Show,Eq)
data Comparator = LT | GT | LTE | GTE | EQ | NEQ deriving (Show,Eq)
```

and the functions to create them from the matched strings, respectively: 

```haskell
toOperation :: String -> Operation
toOperation "inc" = Inc
toOperation "dec" = Dec
toOperation unmatched = error $ "not a valid operation: " ++ unmatched

toComparator :: String -> Comparator
toComparator "<"  = LT
toComparator ">"  = GT
toComparator "<=" = LTE
toComparator ">=" = GTE
toComparator "==" = EQ
toComparator "!=" = NEQ
toComparator unmatched = error $ "not a valid comparator: " ++ unmatched
```

#### Maps
The registers were also a perfect excuse to use a map as data structure, as each register is composed by its name (a String) and its value (an Int):

```haskell
import qualified Data.Map as Map
type Registers = Map.Map String Int
```


What an intense day, but so rewading! 🏆

## Day 9
One of those days when using Haskell allowed me to complete the challenge in a super short time!

Processing a String (which is nothing more than a [Char] list) is a breeze with pattern matching on both functions parameters and lists!

I find the following declaration super neat, as it's immediately clear what instruction will be run in each different condition:


```
parse :: [Char] -> Int -> Bool -> Int
parse ('{':xs) openedGroups False       = -- open a new group when { is found outside garbage 
parse ('}':xs) openedGroups False       = -- close a group  when } is found outside garbage  
parse ('<':xs) openedGroups False       = -- enter garbage if not already inside
parse ('>':xs) openedGroups True        = -- exit garbage if inside it
parse ('!':xs) openedGroups garbageMode = -- drop next character both inside and outside garbage
parse ( _ :xs) openedGroups garbageMode = -- match every other character (could be a non-relevant one or a curly/angular parenthesis in a position where is not relevant)
parse []       _            _           = -- base recursion step: no more characters to be processed
```

