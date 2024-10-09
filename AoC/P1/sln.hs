-- Advent of Code 2023
-- Problem 1

import Control.Applicative
import Control.Monad

-- Extracts the Int value from the type returned by reads
getNum :: [(Int, String)] -> Int
getNum [(a, b)] = a

-- Returns the first integer value found in the string passed to it
extractNum :: String -> Int 
extractNum [] = -1
extractNum s
    | (reads (take 1 s) :: [(Int, String)]) == [] = extractNum (drop 1 s)
    | True = getNum (reads (take 1 s) :: [(Int, String)])

-- Returns the first integer value found in the reverse of the string parameter
extractNumBack :: String -> Int
extractNumBack s = (extractNum . reverse)(s)

-- Turns the input file string into a list of strings, then returns the 
-- Int values extracted from each line as a list
getArray :: String -> [String] -> [Int]
getArray s [] = getArray s (lines s)
getArray s (x:[]) = [(((extractNum x) * 10) + (extractNumBack x))]
getArray s (x:y) = [(((extractNum x) * 10) + (extractNumBack x))] ++ (getArray s y)

-- Allows us to pipe the the text file to this program for processing
main = do  
    contents <- getContents 
    (print . sum)(getArray contents [])
