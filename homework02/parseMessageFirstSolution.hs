{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

dropArray :: [String] -> String 
dropArray (x:[]) = x

infoTime :: String -> Int 
infoTime x = read((dropArray . drop 1 . take 2 . words)(x))::Int

firstLetter :: String -> String
firstLetter msg = take 1 msg

getInfoMessage :: String -> String
getInfoMessage x = (unwords . drop 2 . words)(x)

getErrorInfo :: String -> LogMessage
getErrorInfo x = LogMessage (Error (read((dropArray . drop 1 . take 2 . words)(x))::Int))
                    (read((dropArray . drop 2 . take 3 . words)(x))::Int) ((unwords . drop 3 . words)(x))

parseMessage :: String -> LogMessage
parseMessage msg
    | (firstLetter msg) == "I" = LogMessage Info (infoTime msg) (getInfoMessage msg)
    | (firstLetter msg) == "W" = LogMessage Warning (infoTime msg) (getInfoMessage msg)
    | (firstLetter msg) == "E" = getErrorInfo msg
    | otherwise = Unknown msg