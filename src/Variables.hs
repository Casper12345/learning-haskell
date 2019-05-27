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

