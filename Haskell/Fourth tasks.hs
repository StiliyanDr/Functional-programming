module FourthTasks where
import Lists
import Prelude hiding (zip, map, all)

sumOfProducts :: Num a => [[a]] -> a
sumOfProducts lists = sumOf products
 where products = map productOf lists

occurrences :: Eq item => [item] -> [item] -> [Int]
occurrences lhs rhs = map (\item -> occurrencesCountOf item rhs) lhs

haveSameLength :: [[item]] -> Bool
haveSameLength [] = True
haveSameLength (first : lists) = all hasLengthOfFirst lists
 where hasLengthOfFirst list = lengthOf list == lengthOfFirst
       lengthOfFirst = lengthOf first

isSorted :: Ord item => [item] -> Bool
isSorted [] = True
isSorted list = all (\(lhs, rhs) -> lhs <= rhs) (zip list (tail list))

isSet :: Ord item => [item] -> Bool
isSet list = isSorted list && list == uniqueElementsOf list

insertInSortedList :: Ord item => item -> [item] -> [item]
insertInSortedList item [] = [item]
insertInSortedList item (head : tail) =
 if (item < head)
 then (item : head : tail)
 else (head : insertInSortedList item tail)

insertInSet :: Ord item => item -> [item] -> [item]
insertInSet item set = if (isMember item set)
                       then set
		       else insertInSortedList item set

union :: Ord item => [item] -> [item] -> [item]
union lhs rhs = foldRight insertInSet rhs lhs

intersection :: Ord item => [item] -> [item] -> [item]
intersection lhs rhs = [ item | item <- union lhs rhs,
                                isMember item lhs,
				isMember item rhs]

difference :: Eq item => [item] -> [item] -> [item]
difference lhs rhs = [item | item <- lhs, not (isMember item rhs)]

