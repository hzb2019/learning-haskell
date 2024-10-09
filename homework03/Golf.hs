-- This function takes an Int a and an array x:y,
-- it extracts characters skipping (a - 1) characters
-- per extraction
-- ex.  spl 1 "ABCD" -> "ABCD"
--      spl 2 "ABCD" -> "BD"
--      spl 3 [1, 2, 3, 4, 5, 6] -> [3, 6]
--      spl 4 [1, 2, 3, 4, 5, 6] -> [4]
spl :: Int -> [a] -> [a]
spl _ [] = []
spl a (x:y) = (drop (a - 1) (take a (x:y))) ++ (spl a (drop a (x:y)))

-- This function takes a polymorphic array x and an 
-- array of Int values (y:z) and calls spl for each
-- value in (y:z) 
callSpl :: [a] -> [Int] -> [[a]]
callSpl _ [] = []
callSpl x (y:z) = [(spl y x)] ++ callSpl x z

-- This function uses the variable x to represent
-- a polymorphic array, passes the full array to 
-- callSpl, then obtains a list of Ints from 
-- 1 to length x and passes that as the second
-- argument to callSpl
skips :: [a] -> [[a]]
skips [] = []
skips [x] = [x]:[]
skips x = callSpl x ([1.. (length x)])

-- spl: 2 lines of code, 84 characters 
-- callSpl: 2 lines of code, 63 characters
-- skips: 3 lines of code, 73 characters
-- Total count: 7 lines of code, 220 characters

-- Takes a list of Integers of length 3 -
-- otherwise it returns False by default -
-- gets the maximum value of the numbers, 
-- then checks to see if that value is the
-- middle value in the list
chkLocMax :: [Integer] -> Bool
chkLocMax x 
    | not (length x == 3) = False
    | otherwise = maximum x == x !! 1

-- Iterates through the list of Integers, using
-- chkLocMax to determine if the middle number 
-- should be appended to the list, if so the 
-- next call is incremented by 2 indices, otherwise
-- the next call is incremented by 1
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima (x:y:z)
    | chkLocMax (take 3 (x:y:z)) = [y] ++ localMaxima (z)
    | otherwise = localMaxima (y:z)

-- note, some characters may be regained if 'True' is used to
-- substitute for 'otherwise'
-- chkLocMax: 3 lines, 86 characters
-- localMaxima: 4 lines, 158 characters
-- Total count: 7 lines, 244 characters

-- takes single integer value and adds it as an x in the proper
-- position in the string, then returns the string
addXAt :: String -> Int -> String
addXAt string position = (take position string) ++ "*" ++ (drop (position + 1) string)

-- checks for a specific character at the given position in the provided string
charIsAt :: String -> Int -> Char -> Bool
charIsAt string position character = head (drop position string) == character

isXAt :: Int -> String -> Bool
isXAt position string = charIsAt string position '*'

-- adds an empty string layer to be filled with x's
addNewLayer :: [String] -> [String]
addNewLayer stringList = stringList ++ ["          "]

stackFst :: Int -> [String] -> [String]
stackFst position stringList = fst (span (isXAt position) stringList)

stackSnd :: Int -> [String] -> [String]
stackSnd position stringList = snd (span (isXAt position) stringList)

needsNewLayer :: Int -> [String] -> Bool
needsNewLayer position stringList = length (stackSnd position stringList) == 0

addXToNextLayer :: Int -> [String] -> [String]
addXToNextLayer position stringList = if(needsNewLayer position stringList)
                                        then (stackFst position stringList) ++ [addXAt "          " position]
                                        else (stackFst position stringList) ++ [addXAt (head (stackSnd position stringList)) position] ++ (drop 1 (stackSnd position stringList))

convertStackToString :: [String] -> String
convertStackToString [] = ""
convertStackToString (x:y) = x ++ "\n" ++ convertStackToString y

histogramStack :: [Integer] -> [String] -> String
histogramStack [] stringStack = convertStackToString (reverse stringStack) ++ "==========\n0123456789\n"
histogramStack (x:y) stringStack = histogramStack y (addXToNextLayer (fromIntegral x) stringStack)

integerListToString :: [Integer] -> String
integerListToString [] = ""
integerListToString (x:[]) = (show x)
integerListToString (x:y) = (show x) ++ "," ++ integerListToString y

histogram :: [Integer] -> String
histogram intList = "histogram [" ++ integerListToString intList ++ "] ==\n" ++ histogramStack intList []

