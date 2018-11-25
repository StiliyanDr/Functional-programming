module Basics where

plusN :: Int -> (Int -> Int)
plusN n = (+) n

increment :: Int -> Int
increment number = (plusN 1) number

decrement :: Int -> Int
decrement number = (plusN (- 1)) number

square :: Double -> Double
square = (** 2)

isPositive :: Int -> Bool
isPositive = (> 0)

isZero :: Int -> Bool
isZero = (== 0)

isNonzero :: Int -> Bool
isNonzero = (/= 0)

lastDigit :: Int -> Int
lastDigit = (`mod` 10)

stripDigit :: Int -> Int
stripDigit = (`div` 10)

absolute :: Int -> Int
absolute number
 | isPositive number = number
 | otherwise = (- number)
