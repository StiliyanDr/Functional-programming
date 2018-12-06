module Lists where
import Prelude hiding (all, any, reverse, append, id, map, filter)
import Basics

isEmpty :: [a] -> Bool
isEmpty = null

tailOf :: [a] -> [a]
tailOf (head : tail) = tail
tailOf [] = error "The empty list has no tail!"

headOf :: [a] -> a
headOf (head : tail) = head
headOf [] = error "The empty list has no head!"

foldLeft :: (t1 -> t -> t1) -> t1 -> [t] -> t1
foldLeft operation neutralElement list
 | isEmpty list = neutralElement
 | otherwise = foldLeft operation subresult rest
    where subresult = operation neutralElement (headOf list)
          rest = tailOf list

foldRight :: (t -> t1 -> t1) -> t1 -> [t] -> t1
foldRight operation neutralElement list =
 if (isEmpty list)
 then neutralElement
 else (operation (headOf list) subresult)
  where subresult = foldRight operation neutralElement (tailOf list)

lengthOf :: [a] -> Int
lengthOf = foldLeft increment 0
 where increment length item = length + 1

reverse :: [a] -> [a]
reverse = foldLeft insertFront []
 where insertFront list item = (item : list)

append :: [a] -> [a] -> [a]
append [] rhs = rhs
append (head : tail) rhs = (head : appended)
 where appended = append tail rhs

id :: t -> t
id x = x

map :: (a -> b) -> [a] -> [b]
map function list = reverse (foldLeft operation [] list)
 where operation mapped item = (function item) : mapped

filter :: (a -> Bool) -> [a] -> [a]
filter predicate list = reverse (foldLeft insertPassing [] list)
 where insertPassing filtered item =
        if (predicate item)
        then (item : filtered)else filtered

all :: (item -> Bool) -> [item] -> Bool
all predicate [] = True
all predicate (head : tail) = predicate head && all predicate tail

any :: (item -> Bool) -> [item] -> Bool
any predicate list = not (all complementPredicate list)
 where complementPredicate item = not (predicate item)

isMember :: Eq item => item -> [item] -> Bool
isMember item list = any (\current -> current == item) list

occurancesCountOf :: Eq item => item -> [item] -> Integer
occurancesCountOf item items = foldLeft countOccurances 0 items
 where countOccurances count current = count + select (current == item) 1 0

