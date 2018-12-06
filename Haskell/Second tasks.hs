module SecondTasks where
import Prelude hiding (map, reverse, length, filter, minimum, maximum, replicate)
import Lists 
import Basics

reverseInsert :: [item] -> item -> [item]
reverseInsert tail head = (head : tail)

accumulate :: (t -> t1 -> t) -> t -> Integer -> Integer -> (Integer -> t1) -> (Integer -> Integer) -> t
accumulate operation neutralElement from to term next 
 | from > to = neutralElement
 | otherwise = accumulate operation subresult (next from) to term next
  where subresult = operation neutralElement (term from)

plusOne :: Num a => a -> a
plusOne number = number + 1

replicate :: Integer -> item -> [item]
replicate count item =
 accumulate reverseInsert [] 1 count (\i -> item) plusOne

optimal :: Ord a => (a -> a -> a) -> [a] -> a
optimal optimalOfTwo [] = error "The empty list has no optimal element!"
optimal optimalOfTwo (head : tail) =
 foldl optimalOfTwo head tail

maximum :: Ord a => [a] -> a
maximum = optimal max

minimum :: Ord a => [a] -> a
minimum = optimal min

length :: [item] -> Integer
length = foldl (\len item -> len + 1) 0

divides :: Integer -> Integer -> Bool
divides candidate number = (number `mod` candidate) == 0

divisors :: Integer -> [Integer]
divisors number = [x | x <- [1..number], divides x number]

divisorsCount :: Integer -> Integer
divisorsCount 0 = error "divisorsCount of 0 is not defined!"
divisorsCount number = length (divisors number)

divisorsSum :: Integer -> Integer
divisorsSum 0 = error "divisorsSum of 0 is not defined!"
divisorsSum number = sum (divisors number)

isPrime :: Integer -> Bool
isPrime number = (abs number) > 1 && divisorsCount number == 2

descartes :: [a] -> [b] -> [(a, b)]
descartes lhs rhs = [(a, b) | a <- lhs, b <- rhs]

nats = [0..]
primes = [p | p <- [2..], isPrime p]

natPairs = [(a, sum - a) | sum <- nats, a <- [0..sum]]

sieve :: [Integer] -> [Integer]
sieve (prime : integers) = prime : tailOfPrimes
 where tailOfPrimes = sieve (filter (notDivides prime) integers)
       notDivides number = \candidate -> not (divides number candidate)

sievedPrimes = sieve [2..]

compress :: Eq item => [item] -> [(item, Integer)]
compress [] = []
compress (head : tail) = reverse (utility head 1 tail [])
 where utility current count rest result
        | isEmpty rest = extendedResult
        | head == current = utility current (count + 1) tail result
	| otherwise = utility head 1 tail extendedResult
         where extendedResult = (current, count) : result
	       head = headOf rest
	       tail = tailOf rest

maxRepeated :: Ord item => [item] -> Integer
maxRepeated [] = 0
maxRepeated list = maxCount
 where maxCount = snd (optimal maxPair compressed)
       compressed = compress list
       maxPair lhs@(_, lhsCount) rhs@(_, rhsCount) =
        select (lhsCount > rhsCount) lhs rhs

makeSet :: Eq item => [item] -> [item]
makeSet items = foldl selectUniques [] items
 where selectUniques uniques item =
        select (isMember item uniques) uniques (item : uniques)

type Point = (Double, Double)

sectionLength :: (Point, Point) -> Double
sectionLength ((a, b), (c, d)) =
 sqrt (square (c - a) + square (d - b))

maxDistance :: [Point] -> Double
maxDistance [] = 0
maxDistance points = maximum (map sectionLength pairs)
 where pairs = descartes points points

histogram :: Eq item => [item] -> [(item, Integer)]
histogram items = map makePair uniques
 where uniques = makeSet items
       makePair item = (item, occurancesCountOf item items)
