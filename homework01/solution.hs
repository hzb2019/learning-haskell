-- Takes an Integer and splits it into a
-- list of its digits
toDigits :: Integer -> [Integer]
toDigits x = if (x > 0)
                then toDigits(x `div` 10) ++ [mod x 10]
                else []

-- Takes an Integer and splits it into a
-- reversed list of its digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = if (x > 0)
                then mod x 10 : toDigits (x `div` 10)
                else []

-- Checks to see if the length of the given
-- Integer list is an even number
lenIsEven :: [Integer] -> Bool 
lenIsEven [] = True
lenIsEven (x:[]) = False
lenIsEven (x:y:z) = lenIsEven z

-- Doubles every other Integer in the given 
-- Integer list, from left to right 
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:z)
    | lenIsEven z = [x+x, y] ++ doubleEveryOther z
    | otherwise   = [x, y+y] ++ doubleEveryOther z                

-- Sums each element of the given list
-- [12, 34] -> 46 (12 + 34)
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x : []) = x
sumList (x : y) = x + sumList y    

-- Sums each digit in the given Integer list 
-- [12, 34] -> 10 (1 + 2 + 3 + 4)
sumDigits :: [Integer] -> Integer 
sumDigits [] = 0
sumDigits (x:[]) = sumList (toDigits x)
sumDigits (x : y) = sumList (toDigits x) + sumDigits y

-- validates the given credit card number 
validate :: Integer -> Bool
validate x = if (mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0)
                then True
                else False

numOne = 4012888888881881   -- should evaluate to True
numTwo = 4012888888881882   -- should evaluate to False

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x p1 p2 p3
    | x == 1 = [(p1, p2)]
    | otherwise = (hanoi (x - 1) p1 p3 p2) ++ (hanoi 1 p1 p2 p3) ++ (hanoi (x - 1) p3 p2 p1)
