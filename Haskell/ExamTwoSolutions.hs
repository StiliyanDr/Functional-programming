
module Exam where

-- Task one

row :: [Int]
row = 1 : generate 2 2 row

generate :: Int -> Int -> [Int] -> [Int]
generate currentNumber 0 row =
 generate (currentNumber + 1) (row !! currentNumber) row
generate currentNumber leftCount row =
 currentNumber : (generate currentNumber (leftCount - 1) row)

kolakoskiRow :: [Int]
kolakoskiRow = 1 : generateKolakoski 2 2 2 kol

genKol :: Int -> Int -> Int -> [Int] -> [Int]
genKol current 0 index col =
 genKol (nextNum current)
        (kol !! index)
	(index + 1)
	kol
genKol current leftCount indexOfLast kol =
 current : genKol current (leftCount - 1) index kol

nextNum :: Int -> Int
nextNum 1 = 2
nextNum _ = 1

-- Task two

data NonEmptyTree = Node Int [NonEmptyTree]
data Tree = EmptyTree | NonEmpty NonEmptyTree

haveSimilarFamilies :: Tree -> Tree -> Bool
haveSimilarFamilies EmptyTree EmptyTree = True
haveSimilarFamilies EmptyTree _ = False
haveSimilarFamilies _ EmptyTree = False
haveSimilarFamilies (NonEmpty tree1) (NonEmpty tree2) =
 areSimilar family1 family2
  where family1 = collectFamily tree1
        family2 = collectFamily tree2

collectFamily :: NonEmptyTree -> [Int]
collectFamily (Node root subtrees) =
 root : map (\(Node subroot _) -> subroot) subtrees

areSimilar :: [Int] -> [Int] -> Bool
areSimilar f1 f2 = isSubset f1 f2 && isSubset f2 f1

isSubset :: [Int] -> [Int] -> Bool
isSubset [] _ = True
isSubset (head : tail) set = elem head set && isSubset tail set

-- Task three

--          тактове размер темпо
type Segment = (Int, Int, Int)

-- size = beats / tacts
-- tacts
-- tempo = beats / minute
-- tacts * size -> beats -- / tempo --> minutes

segDuration :: Segment -> Float
segDuration (tacts, size, bmp) =
 fromIntegral (tacts * size) / fromIntegral bmp

type Song = [Segment]

songDuration :: Song -> Float
songDuration song = sum $ map segDuration song

slowestSegs :: Song -> Song
slowestSegs song =
 filter (\(_, _, bmp) -> bpm == slowestBpm) song 
  where slowestBmp = minimum $ map (\(_, _, bpm) -> bpm) song

longestSeg :: Song -> Segment
longestSeg song =
 head $ filter (\seg -> segDuration seg == longestDuration)
  where longestDuration = maximum $ map segDuration song

longestSlowest :: Song -> Segment
longestSlowest = longestSeg . slowestSeg

-- B --

-- Task Two

data NonEmptyTree = NonEmptyTree { rootOf :: Int, subtreesOf :: [NonEmptyTree] }
data Tree = Empty | NonEmpty NonEmptyTree

maxChildOf :: NonEmptyTree -> Int
maxChildOf (NonEmptyTree _ subtrees) =
 maximum $ map rootOf subtrees

rootLineOf :: Tree -> [Int]
rootLineOf Empty = []
rootLineOf (NonEmptyTree root []) = [root]
rootLineOf tree@(NonEmptyTree root subtrees) =
 (root : rootLineOf subtreeOfMaxChild)
  where subtreeOfMaxChild = findSubtreeWithRoot maxChild subtrees
        maxChild = maxChildOf tree
	findSubtreeWithRoot child (subtree : subtrees) =
	 if (child == rootOf subtree)
	 then subtree
	 else findSubtreeWithRoot child subtrees

longestRootLineOf :: Tree -> [Int]
longestRootLineOf Empty = []
longestRootLineOf tree@(NonEmptyTree root subtrees) =
 maximumBy length (rootLineOf tree : longestRootLinesInSubtrees)
  where maxRootLinesInSubtrees = map longesRootLineOf subtrees

maximumBy :: Ord value => ([item] -> value) -> [[item]] -> [item]
maximumBy _ [] = error "No maximum in empty list!"
maximumBy measureOf (head : tail) =
 if (measureOf head > measureOf maxInTail)
 then head
 else maxInTail
  where maxInTail = maximumBy measureOf tail
 
-- Task three

maxNotes :: [[Int]] -> Int
maxNotes [] = 0
maxNotes lists = maximum $ map sum lists

indexOfMax :: (Ord item) => [item] -> Int
indexOfMax [] = -1
indexOfMax [_] = 0
indexOfMax (head : tail) = if (head > maxInTail)
			   then 0
			   else 1 + indexOfMaxInTail
 where maxInTail = tail !! indexOfMaxInTail
       indexOfMaxInTail = indexOfMax tail

uniqueElementsOf :: Eq item => [item] -> [item]
uniqueElementsOf list = foldr insertIfUnique [] list
 where insertIfUnique item uniques = if (item `elem` uniqes)
                                     then uniques
				     else item : uniques

makeAssociativeList :: [item] -> [(item, Int)]
makeAssociativeList list = map (\item -> (item, 0)) list

incrementCountOf :: Eq item => item -> [(item, Int)] -> [(item, Int)]
incrementCountOf item associativeList = map incrementIfEqual associativeList
 where incrementIfEqual (value, count) = (value, count + if (value == item)
                                                         then 1
						         else 0)

itemWithMaxCount :: [(item, Int)] -> item
itemWithMaxCount [] = error "Empty list!"
itemWithMaxCount associativeList = first maxPair
 where maxPair = associativeList !! (indexOfMax counts)
       counts = map second associativeList
       first (a, _) = a
       second (_, b) = b

indexOfLeading :: [[Int]] -> Int
indexOfLeading [] = error "Empty list!"
indexOfLeading ([] : _) = 0
indexOfLeading lists = itemWithMaxCount histogram
 where leadingIndices = map indexOfLeadingAt [0 (lenght $ head lists)]
       indexOfLeadingAt position = indexOfMax $ map (!! position) lists
       associativeList = makeAssociativeList (uniqueElementsOf leadingIndices)
       histogram = foldr incrementCountOf associativeList leadingIndices
