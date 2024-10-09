{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- This method should be more efficient than the one in 
-- parseMessageFirstSolution.hs because this one passes 
-- the entire message along instead of repeating operations
-- in a redundant way. However this way still does not have
-- error handling for every step to ensure validity of strings.


-- To remove a single string element from its container list
-- data structure
dropArray :: [String] -> String 
dropArray (x:[]) = x

-- Gets all information to complete the Error Int form for the
-- MessageType type
getError :: [String] -> MessageType
getError w = Error (read((dropArray . drop 1 . take 2)(w))::Int)

-- Parses the rest of the message and returns the completed LogMessage
parseRest :: (MessageType, [String]) -> LogMessage
parseRest (mt, w) = LogMessage mt (read((dropArray . take 1)(w))::Int)  ((unwords . drop 1)(w))

-- Parses out the MessageType, then passes the rest of the 
-- words along to the parseRest function as a (MessageType, [String])
-- pair where [String] is the words in the Message minus the 
-- words used to determine MessageType (and possibly Error value)
parseType :: [String] -> LogMessage
parseType w 
    | (take 1 w) == ["I"] = parseRest (Info, (drop 1 w))
    | (take 1 w) == ["W"] = parseRest (Warning, (drop 1 w))
    | (take 1 w) == ["E"] = parseRest ((getError w), (drop 2 w))
    | otherwise = Unknown (unwords w)

-- Separates the message into a list of its words, then passes
-- the words along to the parseType function.
parseMessage :: String -> LogMessage
parseMessage msg = (parseType . words)(msg)

-- Passes each string in [String] to the parseMessage function
-- to return a list of the returned values
parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines (l:[]) = (parseMessage l) : []
parseLines (l:n) = (parseMessage l) : parseLines n

-- Separates the lines in the String variable s into 
-- a list of lines, then passes that list to parseLines
parse :: String -> [LogMessage]
parse s = (parseLines . lines)(s)

-- Insert a new LogMessage into an existing MessageTree
-- to return a new MessageTree, or returns the same 
-- MessageTree if the LogMessage is Unknown.
insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left trMsg@(LogMessage _ cmp _) right)
    | ts > cmp && right == Leaf = Node left trMsg (Node Leaf msg Leaf)
    | ts < cmp && left == Leaf  = Node (Node Leaf msg Leaf) trMsg right
    | ts > cmp = Node left trMsg (insert msg right)
    | ts < cmp = Node (insert msg left) trMsg right
insert (Unknown _) tree = tree

-- Build a message tree from a list of messages 
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:[]) = insert x Leaf
build (x:y) = insert x (build y)

-- Return a sorted MessageTree as a list of LogMessages
inorder :: MessageTree -> [LogMessage]
inorder Leaf = []
inorder (Node left msg right) = (inorder left) ++ (msg : []) ++ (inorder right)

-- Adds error messages with severity above 50 to the 
-- returned list of strings
errorMessageExtraction :: [LogMessage] -> [String]
errorMessageExtraction [] = []
errorMessageExtraction ((LogMessage (Error num) _ str):[])
    | num < 50 = []
    | otherwise = str:[]
errorMessageExtraction ((LogMessage (Error num) _ str):y)
    | num < 50 = [] ++ (errorMessageExtraction y)
    | otherwise = str : (errorMessageExtraction y)
errorMessageExtraction (_:y) = errorMessageExtraction y

-- Extract error messages with relevancy above 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = (errorMessageExtraction . inorder . build)(list)

