module Tasks where
import Prelude hiding (cycle, zipWith, zip, unzip, iterate)

type Vector = (Double, Double)

addVectors :: Vector -> Vector -> Vector
addVectors (a, b) (c, d) = (a + c, b + d)

select :: Bool -> t -> t -> t
select condition lhs rhs = if condition then lhs else rhs

isMember :: Eq item => item -> [item] -> Bool
isMember item = any (== item)

removeDuplicates :: Eq item => [item] -> [item]
removeDuplicates list = reverse (foldl selectUniques [] list)
 where selectUniques selected item =
        select (isMember item selected) selected (item : selected)

isSingleton :: [item] -> Bool
isSingleton [_] = True
isSingleton _ = False

compress :: Eq item => [item] -> [item]
compress [] = []
compress list@[_] = list
compress (first:second:rest) =
 select (first == second) compressedTail (first : compressedTail)
  where compressedTail = compress (second : rest)

duplicate :: [item] -> [item]
duplicate = foldr insertTwice []
 where insertTwice item subresult = item:item:subresult

cycle :: [item] -> [item]
cycle [] = []
cycle list = list ++ (cycle list)

quickSort :: Ord item => [item] -> [item]
quickSort [] = []
quickSort (pivot:items) = sortedSmaller ++ (pivot : sortedBigger)
 where sortedSmaller = quickSort [x | x <- items, x < pivot]
       sortedBigger = quickSort [x | x <- items, x >= pivot]

qSort :: Ord item => [item] -> [item]
qSort [] = []
qSort (pivot:items) = sortedSmaller ++ (pivot : sortedBigger)
 where sortedSmaller = qSort (filter (<= pivot) items)
       sortedBigger = qSort (filter (> pivot) items)

insertBack :: item -> [item] -> [item]
insertBack item list = list ++ [item]

merge :: Ord item => [item] -> [item] -> [item]
merge = utility []
 where utility merged [] right = merged ++ right
       utility merged left [] = merged ++ left
       utility merged left@(minLeft:restOfLeft) right@(minRight:restOfRight)
        | minRight < minLeft = utility (insertBack minRight merged) left restOfRight
	| otherwise = utility (insertBack minLeft merged) restOfLeft right

mergeSort :: Ord item => [item] -> [item]
mergeSort [] = []
mergeSort list@[_] = list
mergeSort list = merge sortedLeft sortedRight
 where sortedLeft = mergeSort (fst split)
       sortedRight = mergeSort (snd split)
       split = splitAt middle list
       middle = (length list) `div` 2

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith function (lhs : restOfLhs) (rhs : restOfRhs) =
 (function lhs rhs) : zipWith function restOfLhs restOfRhs

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(u, v) (lhs, rhs) -> (u : lhs, v : rhs)) ([], [])

iterate :: Integral a => (a -> b) -> a -> [b]
iterate function from = map function [from ..]
