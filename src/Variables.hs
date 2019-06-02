module Variables where

import Prelude hiding (length)

-- local variables with where
lend amount balance = if balance < reserve then Nothing else Just balance
                      where reserve = 100
                            newBalance = amount - balance


-- local variables with let

lend2 amount balance = let reserve = 100
                           newBalance = amount - balance
                       in if balance < reserve then Nothing else Just balance

-- pattern matching

data Fruit = Orange | Apple | Banana | Unidentified
    deriving (Show)

-- using case statement

select1 fruit = case fruit of
                 "orange" -> Orange
                 "apple" -> Apple
                 "banana" -> Banana
                 _ -> Unidentified

-- using regular matching

select "orange" = Orange
select "apple" = Apple
select "banana" = Banana
select _ = Unidentified

-- exercises 3

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

mean :: [Int] -> Double
mean n = fromIntegral (sum n) / fromIntegral (length n)

-- all functions only take one argument currying

dropWhileIsTwo :: [Int] -> [Int]
dropWhileIsTwo = dropWhile (== 2)

niceSum :: [Int] -> Int
niceSum xs = foldl (+) 0 xs

addThree :: [Int] -> [Int]
addThree xs = map (+3) xs

-- as pattern

suffixes :: [a] -> [[a]]
suffixes xs@(_ :tail) = xs : suffixes tail
suffixes _ = []

add x y = x + y
addInfix x y = x `add` y

