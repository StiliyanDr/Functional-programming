module Lists where
import Prelude hiding (map, filter, zip, zipWith, unzip, and, or, reverse, all, any, repeat, replicte, cycle, iterate)

isEmpty :: [a] -> Bool
isEmpty = null

tailOf :: [a] -> [a]
tailOf (_ : tail) = tail
tailOf [] = error "The empty list has no tail!"

headOf :: [a] -> a
headOf (head : _) = head
headOf [] = error "The empty list has no head!"

foldLeft :: (r -> t -> r) -> r -> [t] -> r
foldLeft _ neutralElement [] = neutralElement
foldLeft operation neutralElement (head : tail) =
 foldLeft operation subresult tail
  where subresult = operation neutralElement head

foldLeft1 :: (a -> a -> a) -> [a] -> a
foldLeft1 operation (head : tail) = foldLeft operation head tail
  
foldRight :: (t -> r -> r) -> r -> [t] -> r
foldRight _ neutralElement [] = neutralElement
foldRight operation neutralElement (head : tail) = operation head subresult
  where subresult = foldRight operation neutralElement tail

foldRight1 :: (a -> a -> a) -> [a] -> a
foldRight1 _ [a] = a
foldRight1 operation (head : tail) = operation head subresult
 where subresult = foldRight1 operation tail

foldLeftStrict :: (r -> a -> r) -> r -> [a] -> r
foldLeftStrict _ neutralElement [] = neutralElement
foldLeftStrict operation neutralElement (head : tail) =
 (foldLeftStrict operation $! (operation neutralElement head)) tail

and :: [Bool] -> Bool
and = foldLeft (&&) True

or :: [Bool] -> Bool
or = foldRight (||) False

sumOf :: Num a => [a] -> a
sumOf list = foldLeftStrict (+) 0 list

productOf :: Num a => [a] -> a
productOf list = foldLeftStrict (*) 1 list

concatenate :: [[item]] -> [item]
concatenate lists = foldRight (++) [] lists

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith function (left : lhs) (right : rhs) =
 (function left right) : (zipWith function lhs rhs)

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

unzip :: [(a, b)] -> ([a], [b])
unzip pairs =
 foldRight (\(a, b) (lhs, rhs) -> (a : lhs, b : rhs)) ([], []) pairs

lengthOf :: [a] -> Int
lengthOf list = foldLeftStrict (\count _ -> count + 1) 0 list

reverse :: [a] -> [a]
reverse = foldLeft (\reversed item -> (item : reversed)) []

append :: [a] -> [a] -> [a]
append [] rhs = rhs
append (head : tail) rhs = (head : appended)
 where appended = append tail rhs

id :: t -> t
id = \x -> x

map :: (a -> b) -> [a] -> [b]
map function list = foldRight operation [] list
 where operation item mapped = (function item) : mapped

filter :: (a -> Bool) -> [a] -> [a]
filter predicate list = foldRight insertIfPassing [] list
 where insertIfPassing item filtered =
        if (predicate item)
        then (item : filtered)
	else filtered

all :: (item -> Bool) -> [item] -> Bool
all predicate [] = True
all predicate (head : tail) = predicate head && all predicate tail
-- all predicate list = and (map predicate list)

any :: (item -> Bool) -> [item] -> Bool
any predicate list = not (all complementPredicate list)
 where complementPredicate item = not (predicate item)
-- any predicate list = or (map predicate list)

isMember :: Eq item => item -> [item] -> Bool
isMember item list = any (== item) list

occurrencesCountOf :: Eq item => item -> [item] -> Int
occurrencesCountOf item items = sumOf bits
 where bits = map (\current -> if (current == item) then 1 else 0) items
 
uniqueElementsOf :: Eq item => [item] -> [item]
uniqueElementsOf list = foldRight insertIfUnique [] list
 where insertIfUnique item uniques =
        if (isMember item uniques)
	    then uniques
	    else (item : uniques)

repeat :: item -> [item]
repeat item = (item : repeat item)

replicate :: Int -> item -> [item]
replicate times item = take times (repeat item)

cycle :: [item] -> [item]
cycle [] = []
cycle list = list ++ (cycle list)

iterate :: (a -> a) -> a -> [a]
iterate function item = (item : iterate function (function item))

