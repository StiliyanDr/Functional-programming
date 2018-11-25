module FirstTasks where
import Basics

fastExp :: Double -> Int -> Double
fastExp number 0 = 1
fastExp number power
 | power < 0 = 1 / (fastExp number (- power))
 | even power = square (fastExp number (power `div` 2))
 | otherwise = number * (fastExp number (decrement power))

factorial :: Int -> Int
factorial = fact 1
 where fact result 0 = result
       fact result number
        | number > 0 = fact (number * result) (number - 1)
	| otherwise = error "Number must be nonegative!"

fibonacci :: Int -> Int
fibonacci = fib 0 1
 where fib current next 0 = current
       fib current next n
	| isPositive n = fib next (current + next) (n - 1)
	| otherwise = error "Argument must be nonegative!"

reverseInteger :: Int -> Int
reverseInteger number =
 if number >= 0
 then reverse 0 number
 else - (reverse 0 (- number))
  where reverse result 0 = result
        reverse result rest = reverse extendedResult shrinkedRest
         where extendedResult = (10 * result) + lastDigit rest
	       shrinkedRest = stripDigit rest

isPalindrome :: Int -> Bool
isPalindrome number = number == reverseInteger number

divides :: Int -> Int -> Bool
divides 0 number = error "Divison with zero!"
divides candidate number = isZero (number `mod` candidate)

divisorsSum :: Int -> Int
divisorsSum 0 = error "divisorsSum of 0 is not defined!"
divisorsSum number = sumDivisors (abs number) 0
 where sumDivisors 1 result = result + 1
       sumDivisors candidate result = sumDivisors (candidate - 1) (result + addend)
        where addend = if divides candidate number
	               then candidate
		       else 0

myGcd :: Int -> Int -> Int
myGcd 0 0 = error "gcd of 0 and 0 does not exist!"
myGcd a b = helper (abs a) (abs b)
 where helper a 0 = a
       helper 0 b = b
       helper a b
        | a > b = helper (a - b) b
	| otherwise = helper a (b - a)

myLcm :: Int -> Int -> Int
myLcm 0 0 = 0
myLcm a b = (abs (a * b)) `div` (myGcd a b)

ackermann :: Int -> Int -> Int
ackermann m n
 | m < 0 || n < 0 = error "Ackermann function not defined for negative integers!"
 | m == 0 = n + 1
 | n == 0 = ackermann (m - 1) 1
 | otherwise = ackermann (m - 1) (ackermann m (n - 1))

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (a, b) (c, d) = sqrt (square (c - a) + square (d - b))

modulus :: Point -> Double
modulus (a, b) = sqrt (square a + square b)

addComplex :: Point -> Point -> Point
addComplex (a, b) (c, d) = (a + c, b + d)

complexConjugate :: Point -> Point
complexConjugate (a, b) = (a, -b)

subtractComplex :: Point -> Point -> Point
subtractComplex z (a, b) = addComplex z (-a, -b)

multiplyComplex :: Point -> Point -> Point
multiplyComplex (a, b) (c, d) = (a * c - b * d, a * d + b * c)

