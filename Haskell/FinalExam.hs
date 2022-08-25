module FinalExam where

         {- Second task -}

data BinaryTree item = EmptyTree | BinaryTree { root :: item,
                                                leftSubtree :: BinaryTree item,
                                                rightSubtree :: BinaryTree item                                                                     }
					      deriving (Show, Eq, Ord)

makeLeaf :: item -> BinaryTree item
makeLeaf item = BinaryTree item EmptyTree EmptyTree

btree = BinaryTree { root = 1,
                     leftSubtree = BinaryTree { root = 2,
                                                leftSubtree = EmptyTree,
                                                rightSubtree = EmptyTree },
                     rightSubtree = BinaryTree { root = 3,
                                                 leftSubtree = EmptyTree,
                                                 rightSubtree = EmptyTree } }

verticesOf :: BinaryTree item -> [item]
verticesOf EmptyTree = []
verticesOf (BinaryTree root leftSubtree rightSubtree) =
 (root : verticesOf leftSubtree) ++ verticesOf rightSubtree

contains :: Eq item => BinaryTree item -> item -> Bool
contains EmptyTree _ = False
contains (BinaryTree root leftSubtree rightSubtree) item =
 root == item || contains leftSubtree item || contains rightSubtree item

pathCodeOf :: Eq item => item -> BinaryTree item -> Integer
pathCodeOf _ EmptyTree = 0
pathCodeOf item tree = traverse tree 1
  where traverse (BinaryTree root leftSubtree rightSubtree) pathCode
         | root == item = pathCode
	 | contains leftSubtree item =
	  traverse leftSubtree (addDigitTo pathCode 0)
         | contains rightSubtree item =
	  traverse rightSubtree (addDigitTo pathCode 1)
	 | otherwise = 0
        addDigitTo pathCode digit = (pathCode * 10) + digit

numberSystemNToK :: Integer -> Integer -> (Integer -> Integer)
numberSystemNToK n k = \number -> convert number 0 1
 where convert 0 result _ = result
       convert digits result powerOfN =
        convert restOfDigits ((powerOfN * newDigit) + result) (n * powerOfN)
         where newDigit = digits `mod` k
               restOfDigits = digits `div` k

toBinary :: Integer -> Integer
toBinary = numberSystemNToK 10 2

toDecimal :: Integer -> Integer
toDecimal = numberSystemNToK 2 10

verticesSameAsPathCode :: BinaryTree Integer -> [Integer]
verticesSameAsPathCode tree =
 [v | v <- verticesOf tree,  v == (toDecimal $ pathCodeOf v tree)]

verticesSameAsPathCodeCount :: BinaryTree Integer -> Integer
verticesSameAsPathCodeCount = lengthOf . verticesSameAsPathCode

lengthOf :: [item] -> Integer
lengthOf = foldl (\length _ -> length + 1) 0

insertBST :: Ord item => item -> BinaryTree item -> BinaryTree item
insertBST item EmptyTree = makeLeaf item
insertBST item (BinaryTree root leftSubtree rightSubtree) =
 if (item < root)
 then BinaryTree root (insertBST item leftSubtree) rightSubtree
 else BinaryTree root leftSubtree (insertBST item rightSubtree)

listToBST :: Ord item => [item] -> BinaryTree item
listToBST = foldr insertBST EmptyTree

treeToList :: BinaryTree item -> [item]
treeToList EmptyTree = []
treeToList (BinaryTree root leftSubtree rightSubtree) =
 treeToList leftSubtree ++ (root : treeToList rightSubtree)

tree :: BinaryTree Integer
tree = listToBST [1 .. 20]

     {- Third task -}
	 
allEqual :: (Num a, Eq a) => [[a]] -> [(a -> a)] -> [[a]]
allEqual numberLists functions =
 [arguments | arguments <- descartes numberLists, haveEqualValues arguments]
  where haveEqualValues arguments = hasSameElements $ valuesOf arguments
        valuesOf arguments = zipWith ($) functions arguments

arithmeticProgressions :: (Num a, Eq a) => [[a]] -> [(a -> a)] -> [[a]]
arithmeticProgressions numberLists functions =
 [arguments | arguments <- descartes numberLists, isArithmeticProgression $ valuesOf arguments]
  where valuesOf arguments = zipWith ($) functions arguments 

isArithmeticProgression :: (Num a, Eq a) => [a] -> Bool
isArithmeticProgression sequence = hasSameElements deltas
 where deltas = map (\(a, b) -> b - a) pairs
       pairs = zip sequence (tail sequence)

hasSameElements :: Eq item => [item] -> Bool
hasSameElements [] = True
hasSameElements (h : t) = all (== h) t

descartes :: [[item]] -> [[item]]
descartes [] = []
descartes [items] = map makeSingleton items
descartes (h : t) = [x : tuple | x <- h, tuple <- descartes t]

makeSingleton :: item -> [item]
makeSingleton = (: [])

isSingleton :: [item] -> Bool
isSingleton [_] = True
isSingleton _ = False

   {- Task four -}

type Proportion = (Integer, Integer)

numeratorOf, denominatorOf :: Proportion -> Integer
numeratorOf = fst
denominatorOf = snd

makeProportion :: Integer -> Integer -> Proportion
makeProportion a 0 = (a , 0)
makeProportion 0 b = (0 , b)
makeProportion a b = let g = gcd a b in (a `div` g, b `div` g)

data Substance = Substance { nameOf :: String, quantityOf :: Integer }

quantitiesOf :: [Substance] -> [Integer]
quantitiesOf = map quantityOf

instance Eq Substance where
 lhs == rhs = nameOf lhs == nameOf rhs

instance Ord Substance where
 compare lhs rhs = compare (nameOf lhs) (nameOf rhs)

data Medicine = Medicine { medName :: String, substancesOf :: [Substance] }

sortSubstancesOf :: Medicine -> Medicine
sortSubstancesOf (Medicine name substances) =
 (Medicine name $ quickSort substances)

quickSort :: Ord item => [item] -> [item]
quickSort [] = []
quickSort (pivot : items) = sortedSmaller ++ pivot : sortedGreater
 where sortedSmaller = quickSort $ filter (<= pivot) items
       sortedGreater = quickSort $ filter (> pivot) items

isSubstitute :: Medicine -> Medicine -> Bool
isSubstitute lhs rhs =
 sortedSubsLhs == sortedSubsRhs
  && haveSameProportions sortedSubsLhs sortedSubsRhs
   where (Medicine _ sortedSubsLhs) = sortSubstancesOf lhs
         (Medicine _ sortedSubsRhs) = sortSubstancesOf rhs

haveSameProportions :: [Substance] -> [Substance] -> Bool
haveSameProportions lhs rhs = hasSameElements (proportionsOf lhs rhs)

proportionsOf :: [Substance] -> [Substance] -> [Proportion]
proportionsOf lhs rhs =
 zipWith makeProportion (quantitiesOf lhs) (quantitiesOf rhs)

medOne, medTwo, medThree :: Medicine
medOne = (Medicine "One" [Substance "a" 3, Substance "c" 8, Substance "b" 5])
medTwo = (Medicine "Two" [Substance "b" 15, Substance "a" 9, Substance "c" 24])
medThree = (Medicine "Three" [Substance "a" 3, Substance "b" 6, Substance "c" 8])
medFour = (Medicine "Four" [Substance "a" 3, Substance "c" 8, Substance "b" 5])
medFive = (Medicine "Five" [Substance "b" 12, Substance "c" 10])

data Pair = Pair { pairName :: String, pairDifference :: Integer }

instance Eq Pair where
 (Pair _ lhs) == (Pair _ rhs) = lhs == rhs

instance Ord Pair where
 compare (Pair _ lhs) (Pair _ rhs) = compare lhs rhs

bestSubstituteOf :: Medicine -> [Medicine] -> String
bestSubstituteOf medicine medicines = extractName $ minimumPair lessOrEqualDifferencePairs
 where lessOrEqualDifferencePairs =
        [p | p@(Pair _ diff) <- nameAndDifferencePairs, diff >= 0]
       nameAndDifferencePairs = map makePair substitutes
       makePair (Medicine name subs) =
        Pair name (quantityDifferenceOf sortedSubs subs)
       quantityDifferenceOf [] _ = 0
       quantityDifferenceOf (lhs : _) (rhs : _) = quantityOf lhs - quantityOf rhs
       sortedSubs = substancesOf $ sortSubstancesOf medicine
       substitutes = [sortSubstancesOf m | m <- medicines, isSubstitute m medicine]

extractName :: Maybe Pair -> String
extractName Nothing = ""
extractName (Just (Pair name _)) = name

minimumPair :: [Pair] -> Maybe Pair
minimumPair [] = Nothing
minimumPair [pair] = Just pair
minimumPair (h : t) = min (Just h) (minimumPair t)

leastStronger :: Medicine -> [Medicine] -> String
leastStronger medicine medicines = extractName $ minimumPair nameAndDifferencePairs
 where nameAndDifferencePairs =
        [Pair name (differenceOf subs sortedSubs) | (Medicine name subs) <- strongerWithMatchingSubs] 
       differenceOf lhs rhs = sum $ zipWith (-) (quantitiesOf lhs) (quantitiesOf rhs)
       strongerWithMatchingSubs =
        [sortSubstancesOf (leaveOnlyMatchingSubstances m medicine) | m <- stronger] 
       stronger = [m | m <- medicines, isStronger m medicine]
       sortedSubs = substancesOf $ sortSubstancesOf medicine

leaveOnlyMatchingSubstances :: Medicine -> Medicine -> Medicine
leaveOnlyMatchingSubstances (Medicine lhsName lhsSubs) (Medicine _ rhsSubs) =
 (Medicine lhsName [s | s <- lhsSubs, s `elem` rhsSubs])

substitutesPartition :: [Medicine] -> [[Medicine]]
substitutesPartition [] = []
substitutesPartition (m : medicines) = (m : substitutesOfHead) : substitutesPartition rest
 where substitutesOfHead = [s | s <- medicines, isSubstitute s m] 
       rest = [s | s <- medicines, not $ isSubstitute s m]

isStronger :: Medicine -> Medicine -> Bool
isStronger lhs rhs =
 (substancesOf rhs) `isSubset` (substancesOf lhs)
 && hasAtLeastTheSameQuantities
 && hasMoreQuantityOfASubstance
  where hasAtLeastTheSameQuantities = quantitiesLhs >= quantitiesRhs
        hasMoreQuantityOfASubstance =
	 or $ zipWith (>) quantitiesLhs quantitiesRhs
        quantitiesLhs = quantitiesOf matchingSubstances
	matchingSubstances =
	 substancesOf (leaveOnlyMatchingSubstances sortedLhs rhs) 
	sortedLhs = sortSubstancesOf lhs
	quantitiesRhs = quantitiesOf $ substancesOf sortedRhs
	sortedRhs = sortSubstancesOf rhs

strongRelation :: [Medicine] -> [(Medicine, [String])]
strongRelation medicines = foldr insertPair [] medicines
 where insertPair medicine pairs = (medicine, namesOfStronger) : pairs
        where namesOfStronger = map medName stronger
              stronger = [m | m <- medicines, isStronger m medicine]

 {- Set operations -}

isSubset :: Eq item => [item] -> [item] -> Bool
isSubset lhs rhs = all (`elem` rhs) lhs

union :: Eq item => [item] -> [item] -> [item]
union lhs rhs = uniqueElementsOf (lhs ++ rhs)

uniqueElementsOf :: Eq item => [item] -> [item]
uniqueElementsOf = foldr insertIfUnique []
 where insertIfUnique item uniques =
        if (item `elem` uniques)
	then uniques
	else (item : uniques)

intersection :: Eq item => [item] -> [item] -> [item]
intersection lhs rhs =
 [item | item <- union lhs rhs, item `elem` lhs, item `elem` rhs]

